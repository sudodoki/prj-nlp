import tensorflow as tf 
import numpy as np
import pandas as pd
import os

from sklearn.model_selection import train_test_split


tf.app.flags.DEFINE_string('train_dir', '/tmp/kobzar', 'train dir')
tf.app.flags.DEFINE_string('data_path', '', 'data path')

tf.app.flags.DEFINE_integer('epochs', 1, 'num epochs')
tf.app.flags.DEFINE_integer('batch_size', 64, 'batch size')
tf.app.flags.DEFINE_integer('hidden_units', 256, 'num lstm units per layer')
tf.app.flags.DEFINE_integer('num_layers', 2, 'num lstm layers')
tf.app.flags.DEFINE_float('dropout', 0.5, 'dropout keep prob')
tf.app.flags.DEFINE_float('learning_rate', 1e-3, 'learning rate')


FLAGS = tf.app.flags.FLAGS


class GenModel(object):

    def __init__(self, params):
        self.params = params

    def build_graph(self):
        self.input_x = tf.placeholder(tf.int32, [None, None], name='input_x')
        self.input_y = tf.placeholder(tf.int32, [None, None], name='input_y')

        self.dropout_keep_prob = tf.placeholder(tf.float32, name='dropout_keep_prob')

        self.lstm_state = tf.placeholder(tf.float32,
                                         shape=[None, self.params['num_layers'] * 2 * self.params['hidden_units']])

        with tf.name_scope('lstm'):
            def create_cell():
                cell = tf.nn.rnn_cell.LSTMCell(self.params['hidden_units'], state_is_tuple=False)
                cell = tf.nn.rnn_cell.DropoutWrapper(cell, input_keep_prob=self.dropout_keep_prob)
                return cell

            cell = tf.contrib.rnn.MultiRNNCell([create_cell() for _ in range(self.params['num_layers'])],
                                               state_is_tuple=False)

            onehot_x = tf.one_hot(self.input_x, depth=self.params['vocab_size'])
            outputs, self.last_state = tf.nn.dynamic_rnn(cell, onehot_x,
                                                         initial_state=self.lstm_state,
                                                         dtype=tf.float32)

            outputs_flat = tf.reshape(outputs, [-1, self.params['hidden_units']])

        with tf.name_scope('logits'):
            logits = tf.layers.dense(outputs_flat, units=self.params['vocab_size'], activation=None)

            batch_time_shape = tf.shape(outputs)

            preds = tf.nn.softmax(logits)
            y = tf.argmax(preds, 1)
            y = tf.reshape(y, [batch_time_shape[0], -1])

            self.accuracy = tf.reduce_mean(tf.cast(tf.equal(self.input_y, tf.cast(y, tf.int32)), tf.float32))
            self.predictions = tf.reshape(preds, (batch_time_shape[0], batch_time_shape[1], self.params['vocab_size']))

        with tf.name_scope('loss'):
            onehot_y = tf.one_hot(self.input_y, depth=self.params['vocab_size'])
            labels = tf.reshape(onehot_y, [-1, self.params['vocab_size']])
            self.loss = tf.nn.softmax_cross_entropy_with_logits(
                labels=labels,
                logits=logits)
            self.loss = tf.reduce_mean(self.loss)


def main(unused_argv):
    with open(FLAGS.data_path, 'r') as f: 
        lines = f.readlines()
        lines = lines[89:]
        chars = [list(l) for l in lines]
        chars = [l for l in chars if len(l) <= 100]
    
    data = [item for sublist in chars for item in sublist]
    n_chars = len(data)
    vocab = sorted(list(set(data)))
    char_to_idx = {c: i for i, c in enumerate(vocab)}

    sequence_length = 100
    data_x = []
    data_y = []
    for i in range(n_chars - sequence_length - 1):
        seq_in = data[i:i + sequence_length]
        seq_out = data[i + 1:i + sequence_length + 1]
        data_x.append([char_to_idx[e] for e in seq_in])
        data_y.append([char_to_idx[e] for e in seq_out])

    data_x = np.array(data_x)
    data_y = np.array(data_y)
    
    vocab_size = len(vocab)

    print('Vocabulary size: ', vocab_size)
    print('Vocabulary: ', vocab)

    X_train, X_test, y_train, y_test = train_test_split(data_x, data_y, test_size=0.1, random_state=1234)
    
    params = {
        'vocab_size': vocab_size,
        'sequence_length': sequence_length,
        'learning_rate': FLAGS.learning_rate,
        'dropout_keep_prob': FLAGS.dropout,
        'hidden_units': FLAGS.hidden_units,
        'num_layers': FLAGS.num_layers,
    }

    with tf.Graph().as_default():
        model = GenModel(params)
        model.build_graph()

        global_step = tf.Variable(0, name='global_step', trainable=False)
        optimizer = tf.train.AdamOptimizer(FLAGS.learning_rate)
        train_op = optimizer.minimize(model.loss, global_step=global_step)

        saver = tf.train.Saver()
        init_op = tf.global_variables_initializer()

        with tf.Session() as session:
            loss_summary = tf.summary.scalar('loss', model.loss)
            acc_summary = tf.summary.scalar('accuracy', model.accuracy)

            train_summary_op = tf.summary.merge([loss_summary, acc_summary])
            train_summary_dir = os.path.join(FLAGS.train_dir, 'summaries', 'train')
            train_summary_writer = tf.summary.FileWriter(train_summary_dir, session.graph)

            test_summary_op = tf.summary.merge([loss_summary, acc_summary])
            test_summary_dir = os.path.join(FLAGS.train_dir, 'summaries', 'test')
            test_summary_writer = tf.summary.FileWriter(test_summary_dir, session.graph)

            session.run(init_op)

            state = np.zeros((FLAGS.batch_size, FLAGS.num_layers * 2 * FLAGS.hidden_units))

            for epoch in range(FLAGS.epochs):
                idx = np.random.permutation(X_train.shape[0])
                epoch_x = X_train[idx]
                epoch_y = y_train[idx]

                num_batches = int(X_train.shape[0] / FLAGS.batch_size)
                start_idx = 0
                for batch in range(num_batches):
                    x_batch = epoch_x[start_idx:start_idx + FLAGS.batch_size]
                    y_batch = epoch_y[start_idx:start_idx + FLAGS.batch_size]

                    feed_dict = {
                        model.input_x: x_batch,
                        model.input_y: y_batch,
                        model.dropout_keep_prob: FLAGS.dropout,
                        model.lstm_state: state,
                    }
                    _, step, new_state, loss, accuracy, summary = session.run([train_op, global_step, model.last_state,
                                                                               model.loss, model.accuracy,
                                                                               train_summary_op], feed_dict=feed_dict)
                    train_summary_writer.add_summary(summary, step)
                    print('Epoch {}, step: {}, loss {}, accuracy: {}'.format(epoch, step, loss, accuracy))

                    if step != 0 and step % 1000 == 0:
                        print('\nEvaluating on test')

                        test_idx = np.random.permutation(X_test.shape[0])[0:FLAGS.batch_size]
                        x_sample = X_test[test_idx]
                        y_sample = y_test[test_idx]

                        loss, accuracy, summary = session.run([model.loss, model.accuracy,
                                                               test_summary_op], feed_dict={
                            model.input_x: x_sample,
                            model.input_y: y_sample,
                            model.dropout_keep_prob: 1,
                            model.lstm_state: np.zeros((x_sample.shape[0], FLAGS.num_layers * 2 * FLAGS.hidden_units)),
                        })
                        test_summary_writer.add_summary(summary, step)
                        print('Test loss: {}, accuracy: {}'.format(loss, accuracy))
                        saver.save(session, os.path.join(FLAGS.train_dir, 'model_{}.ckpt'.format(step)))

                    state = new_state
                    start_idx += FLAGS.batch_size

            saver.save(session, os.path.join(FLAGS.train_dir, 'model_final.ckpt'))


if __name__ == '__main__':
    tf.logging.set_verbosity(tf.logging.INFO)
    tf.app.run()
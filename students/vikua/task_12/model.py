import tensorflow as tf 
import numpy as np
import pandas as pd
import os

from sklearn.model_selection import train_test_split


tf.app.flags.DEFINE_string('train_dir', '/tmp/kobzar', 'train dir')
tf.app.flags.DEFINE_string('data_path', '', 'data path')

tf.app.flags.DEFINE_integer('epochs', 1, 'num epochs')
tf.app.flags.DEFINE_integer('batch_size', 128, 'batch size')
tf.app.flags.DEFINE_integer('hidden_units', 128, 'num lstm units per layer')
tf.app.flags.DEFINE_integer('num_layers', 2, 'num lstm layers')
tf.app.flags.DEFINE_float('dropout', 0.2, 'dropout keep prob')
tf.app.flags.DEFINE_float('learning_rate', 1e-2, 'learning rate')
tf.app.flags.DEFINE_integer('embedding_dim', 100, 'embedding demension')


FLAGS = tf.app.flags.FLAGS


def model1(features, labels, mode, params):
    #char_vectors = tf.contrib.layers.embed_sequence(features['x'], 
    #                                                vocab_size=params['vocab_size'],
    #                                                embed_dim=params['embed_dim'])
    char_vectors = tf.one_hot(features['x'], depth=params['vocab_size'])
    
    with tf.name_scope('lstm'):
        lsmt_cells = [
            tf.nn.rnn_cell.LSTMCell(params['hidden_units'], state_is_tuple=True)
            for _ in range(params['num_layers'])
        ]
        cell = tf.contrib.rnn.MultiRNNCell(lsmt_cells, state_is_tuple=True)
        outputs, last_state = tf.nn.dynamic_rnn(cell, char_vectors, dtype=tf.float32)
        output = outputs[-1]

    with tf.name_scope('logits'): 
        logits = tf.layers.dense(output, units=params['vocab_size'], activation=None)
        predictions = tf.nn.softmax(logits)
        
    if mode == tf.estimator.ModeKeys.PREDICT: 
        return tf.estimator.EstimatorSpec(mode, predictions={'class': predictions})

    with tf.name_scope('loss'):
        onehot_y = tf.one_hot(labels, depth=params['vocab_size'], dtype=tf.float32)
        cross_entropy = tf.nn.softmax_cross_entropy_with_logits(
            labels=onehot_y,
            logits=logits)
        loss = tf.reduce_mean(cross_entropy)
    
    if mode == tf.estimator.ModeKeys.TRAIN: 
        optimizer = tf.train.AdamOptimizer(params['learning_rate'])
        train_op = optimizer.minimize(loss, global_step=tf.train.get_global_step())
        return tf.estimator.EstimatorSpec(mode=mode, loss=loss, train_op=train_op)

    eval_metric_ops = {
        'accuracy': tf.metrics.accuracy(labels=labels, predictions=predictions)
    }
    return tf.estimator.EstimatorSpec(mode=mode, loss=loss, eval_metric_ops=eval_metric_ops)


class GenModel(object):

    def __init__(self, params):
        self.params = params

    def build_graph(self):
        self.input_x = tf.placeholder(tf.int32, [None, None], name='input_x')
        self.input_y = tf.placeholder(tf.int32, [None, None], name='input_y')

        self.lstm_state = tf.placeholder(tf.float32,
                                         shape=[None, self.params['num_layers'] * 2 * self.params['hidden_units']])

        with tf.name_scope('lstm'):
            onehot_x = tf.one_hot(self.input_x, depth=self.params['vocab_size'])
            cells = [
                tf.nn.rnn_cell.LSTMCell(self.params['hidden_units'], state_is_tuple=False)
                for _ in range(self.params['num_layers'])
            ]
            cell = tf.contrib.rnn.MultiRNNCell(cells, state_is_tuple=False)

            outputs, self.last_state = tf.nn.dynamic_rnn(cell, onehot_x,
                                                         initial_state=self.lstm_state,
                                                         dtype=tf.float32)
            outputs_reshaped = tf.reshape(outputs, [-1, self.params['hidden_units']])

        with tf.name_scope('logits'):
            logits = tf.layers.dense(outputs_reshaped, units=self.params['vocab_size'], activation=None)
            batch_time_shape = tf.shape(outputs)
            soft_max_preds = tf.nn.softmax(logits)
            self.predictions = tf.reshape(soft_max_preds,
                                          (batch_time_shape[0], batch_time_shape[1], self.params['vocab_size']))

        with tf.name_scope('loss'):
            onehot_y = tf.one_hot(self.input_y, depth=self.params['vocab_size'])
            labels = tf.reshape(onehot_y, [-1, self.params['vocab_size']])
            cross_entropy = tf.nn.softmax_cross_entropy_with_logits(
                labels=labels,
                logits=logits)
            self.loss = tf.reduce_mean(cross_entropy)


def main(unused_argv):
    with open(FLAGS.data_path, 'r') as f: 
        lines = f.readlines()
        lines = lines[89:]
        chars = [list(l.lower()) for l in lines]
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

    X_train, X_test, y_train, y_test = train_test_split(data_x, data_y, test_size=0.1, random_state=1234)
    
    params = {
        'vocab_size': vocab_size,
        'sequence_length': sequence_length,
        'learning_rate': FLAGS.learning_rate,
        'dropout_keep_prob': FLAGS.dropout,
        'hidden_units': FLAGS.hidden_units,
        'num_layers': FLAGS.num_layers,
        'embed_dim': FLAGS.embedding_dim
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

            train_summary_dir = os.path.join(FLAGS.train_dir, 'summaries', 'train')
            train_summary_writer = tf.summary.FileWriter(train_summary_dir, session.graph)

            test_summary_dir = os.path.join(FLAGS.train_dir, 'summaries', 'test')
            test_summary_writer = tf.summary.FileWriter(test_summary_dir, session.graph)

            session.run(init_op)

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
                        model.lstm_state: np.zeros((x_batch.shape[0], FLAGS.num_layers * 2 * FLAGS.hidden_units)),
                    }
                    _, step, loss, summary = session.run([train_op, global_step, model.loss, loss_summary],
                                                         feed_dict=feed_dict)
                    train_summary_writer.add_summary(summary, step)
                    print('Epoch {}, step: {}, loss {}'.format(epoch, step, loss))

                    if step != 0 and step % 1000 == 0:
                        print('\nEvaluating on test')

                        loss, summary = session.run([model.loss, loss_summary], feed_dict={
                            model.input_x: X_test,
                            model.input_y: y_test,
                            model.lstm_state: np.zeros((X_test.shape[0], FLAGS.num_layers * 2 * FLAGS.hidden_units)),
                        })
                        test_summary_writer.add_summary(summary, step)
                        print('Test loss: ', loss)
                        saver.save(session, os.path.join(FLAGS.train_dir, 'model.ckpt'))

                    start_idx += FLAGS.batch_size

            saver.save(session, os.path.join(FLAGS.train_dir, 'model.ckpt'))


if __name__ == '__main__':
    tf.logging.set_verbosity(tf.logging.INFO)
    tf.app.run()
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
tf.app.flags.DEFINE_float('learning_rate', 1e-3, 'learning rate')
tf.app.flags.DEFINE_integer('embedding_dim', 100, 'embedding demension')

FLAGS = tf.app.flags.FLAGS


def model(features, labels, mode, params):
    char_vectors = tf.one_hot(features['x'], depth=params['vocab_size'])

    with tf.name_scope('lstm'):
        lsmt_cells = [
            tf.nn.rnn_cell.LSTMCell(params['hidden_units'], state_is_tuple=True)
            for _ in range(params['num_layers'])
        ]
        cell = tf.contrib.rnn.MultiRNNCell(lsmt_cells, state_is_tuple=True)
        outputs, last_state = tf.nn.dynamic_rnn(cell, char_vectors,
                                                dtype=tf.float32)
        outputs = tf.transpose(outputs, [1, 0, 2])

    with tf.name_scope('logits'):
        logits = tf.layers.dense(outputs[-1], units=params['vocab_size'], activation=None)
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
    for i in range(n_chars - sequence_length):
        seq_in = data[i:i + sequence_length]
        seq_out = data[i + sequence_length]
        data_x.append([char_to_idx[e] for e in seq_in])
        data_y.append(char_to_idx[seq_out])

    data_x = np.array(data_x)
    data_y = np.array(data_y)

    vocab_size = len(vocab)

    X_train, X_test, y_train, y_test = train_test_split(data_x, data_y, test_size=0.1, random_state=1234)

    params = {
        'vocab_size': vocab_size,
        'learning_rate': FLAGS.learning_rate,
        'dropout_keep_prob': FLAGS.dropout,
        'hidden_units': FLAGS.hidden_units,
        'num_layers': FLAGS.num_layers,
        'embed_dim': FLAGS.embedding_dim
    }
    classifier = tf.estimator.Estimator(model_fn=model, params=params, model_dir=FLAGS.train_dir)

    train_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={'x': X_train},
        y=y_train,
        num_epochs=FLAGS.epochs,
        batch_size=FLAGS.batch_size,
        shuffle=True
    )
    classifier.train(input_fn=train_input_fn)

    test_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={'x': X_test},
        y=y_test,
        num_epochs=1,
        batch_size=y_test.shape[0],
        shuffle=False
    )
    eval_results = classifier.evaluate(input_fn=test_input_fn)
    print('Evaluation results: {}'.format(eval_results))


if __name__ == '__main__':
    tf.logging.set_verbosity(tf.logging.INFO)
    tf.app.run()
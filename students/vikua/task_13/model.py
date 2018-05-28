import tensorflow as tf
import pandas as pd
import numpy as np


from sklearn.model_selection import train_test_split
from sklearn.metrics import f1_score, classification_report, confusion_matrix


tf.app.flags.DEFINE_string('train_dir', '/tmp/twitter', 'train dir')
tf.app.flags.DEFINE_string('data_path', '', 'data path')
tf.app.flags.DEFINE_integer('epochs', 1, 'number of epochs')
tf.app.flags.DEFINE_integer('batch_size', 128, 'batch size')
tf.app.flags.DEFINE_integer('hidden_units', 256, 'hidden dimension')
tf.app.flags.DEFINE_integer('embed_dim', 300, 'embedding dimension')
tf.app.flags.DEFINE_float('learning_rate', 0.001, 'learning rate')

FLAGS = tf.app.flags.FLAGS


def _mean_pool(input_tensor, seq_len):
    input_sum = tf.reduce_sum(input_tensor, axis=1)
    expanded_seq_len = tf.cast(tf.expand_dims(seq_len, -1), tf.float32) + 1e-08
    mean_pooled = (input_sum / expanded_seq_len)
    return mean_pooled


def model(features, labels, mode, params):
    s1_embedded = tf.contrib.layers.embed_sequence(features['s1'],
                                                   vocab_size=params['vocab_size'],
                                                   embed_dim=params['embed_dim'])

    s2_embedded = tf.contrib.layers.embed_sequence(features['s2'],
                                                   vocab_size=params['vocab_size'],
                                                   embed_dim=params['embed_dim'])

    with tf.variable_scope('s1_bi_lstm'):
        s1_fw_cell = tf.nn.rnn_cell.LSTMCell(params['hidden_units'], state_is_tuple=True)
        s1_bw_cell = tf.nn.rnn_cell.LSTMCell(params['hidden_units'], state_is_tuple=True)
        (s1_fw_outputs, s1_bw_outputs), _ = tf.nn.bidirectional_dynamic_rnn(s1_fw_cell, s1_bw_cell, s1_embedded,
                                                                            sequence_length=features['s1_len'],
                                                                            dtype=tf.float32)

        s1_fw_outputs = _mean_pool(s1_fw_outputs, features['s1_len'])
        s1_bw_outputs = _mean_pool(s1_bw_outputs, features['s1_len'])
        encoded_s1 = tf.concat([s1_fw_outputs, s1_bw_outputs], axis=1)

    with tf.variable_scope('s2_bi_lstm'):
        s2_fw_cell = tf.nn.rnn_cell.LSTMCell(params['hidden_units'], state_is_tuple=True)
        s2_bw_cell = tf.nn.rnn_cell.LSTMCell(params['hidden_units'], state_is_tuple=True)
        (s2_fw_outputs, s2_bw_outputs), _ = tf.nn.bidirectional_dynamic_rnn(s2_fw_cell, s2_bw_cell, s2_embedded,
                                                                            sequence_length=features['s2_len'],
                                                                            dtype=tf.float32)

        s2_fw_outputs = _mean_pool(s2_fw_outputs, features['s2_len'])
        s2_bw_outputs = _mean_pool(s2_bw_outputs, features['s2_len'])
        encoded_s2 = tf.concat([s2_fw_outputs, s2_bw_outputs], axis=1)

    with tf.name_scope('match'):
        diff = encoded_s1 - encoded_s2
        prod = encoded_s1 * encoded_s2
        matching_vec = tf.concat([encoded_s1, prod, diff, encoded_s2], axis=1)

    with tf.name_scope('projection'):
        projection = tf.layers.dense(matching_vec, units=2, activation=tf.nn.relu)
        probabilities = tf.nn.softmax(projection)
        predictions = tf.argmax(probabilities, axis=1)

    if mode == tf.estimator.ModeKeys.PREDICT:
        return tf.estimator.EstimatorSpec(mode=mode, predictions={'proba': probabilities,
                                                                  'class': predictions})

    loss = tf.nn.sparse_softmax_cross_entropy_with_logits(logits=projection, labels=labels)
    loss = tf.reduce_mean(loss)

    if mode == tf.estimator.ModeKeys.TRAIN:
        optimizer = tf.train.AdamOptimizer(params['learning_rate'])
        train_op = optimizer.minimize(loss, global_step=tf.train.get_global_step())
        return tf.estimator.EstimatorSpec(mode=mode, loss=loss, train_op=train_op)

    eval_metric_ops = {
        'accuracy': tf.metrics.accuracy(labels=labels, predictions=predictions),
        'recall': tf.metrics.recall(labels=labels, predictions=predictions),
        'precision': tf.metrics.precision(labels=labels, predictions=predictions),
    }
    return tf.estimator.EstimatorSpec(mode=mode, loss=loss, eval_metric_ops=eval_metric_ops)


def main(unused_argv):
    df = pd.read_parquet(FLAGS.data_path)
    print('Data loaded')

    X_train, X_test, y_train, y_test = train_test_split(df[['s1', 's2']], df['y'],
                                                        test_size=0.1, stratify=df['y'], random_state=1234)

    sentences = pd.concat([X_train['s1'], X_train['s2']], ignore_index=True)

    max_doc_length = max([len(x) for x in sentences])
    vocab_processor = tf.contrib.learn.preprocessing.VocabularyProcessor(max_doc_length,
                                                                         tokenizer_fn=lambda x: x)
    vocab_processor.fit(sentences)

    vocab_size = len(vocab_processor.vocabulary_._mapping)

    print('Vocabulary processor created, size {}'.format(vocab_size))
    print('Max sequence length: {}'.format(max_doc_length))

    X_train_s1 = np.array(list(vocab_processor.transform(X_train['s1'].values)))
    X_train_s2 = np.array(list(vocab_processor.transform(X_train['s2'].values)))

    X_train_s1_len = np.array([len(x) for x in X_train['s1'].values])
    X_train_s2_len = np.array([len(x) for x in X_train['s2'].values])

    X_test_s1 = np.array(list(vocab_processor.transform(X_test['s1'].values)))
    X_test_s2 = np.array(list(vocab_processor.transform(X_test['s2'].values)))

    X_test_s1_len = np.array([len(x) for x in X_test['s1'].values])
    X_test_s2_len = np.array([len(x) for x in X_test['s2'].values])

    params = {
        'vocab_size': vocab_size,
        'embed_dim': FLAGS.embed_dim,
        'hidden_units': FLAGS.hidden_units,
        'learning_rate': FLAGS.learning_rate,
    }
    classifier = tf.estimator.Estimator(model_fn=model, params=params, model_dir=FLAGS.train_dir)

    train_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={'s1': X_train_s1,
           's2': X_train_s2,
           's1_len': X_train_s1_len,
           's2_len': X_train_s2_len},
        y=y_train.values,
        num_epochs=FLAGS.epochs,
        batch_size=FLAGS.batch_size,
        shuffle=True
    )
    classifier.train(input_fn=train_input_fn)

    test_input_fn = tf.estimator.inputs.numpy_input_fn(
        x={'s1': X_test_s1,
           's2': X_test_s2,
           's1_len': X_test_s1_len,
           's2_len': X_test_s2_len},
        y=y_test.values,
        num_epochs=1,
        batch_size=FLAGS.batch_size,
        shuffle=False
    )
    classifier.evaluate(input_fn=test_input_fn)

    predictions = classifier.predict(input_fn=test_input_fn)
    y_pred = [p['class'] for p in predictions]
    print('Test F1 score: ', f1_score(y_test.values, y_pred, average='macro'))
    print(classification_report(y_test, y_pred))
    print(confusion_matrix(y_test, y_pred))


if __name__ == '__main__':
    tf.logging.set_verbosity(tf.logging.INFO)
    tf.app.run()
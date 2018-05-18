import numpy as np
from gensim.models import KeyedVectors
from conllu import parse
from sklearn.metrics import log_loss
from keras.models import Model
from keras.layers import Dense, Dropout, Input, LSTM, Bidirectional, Flatten
from keras.optimizers import SGD
from sklearn.preprocessing import label_binarize 

def prepare_data(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        data = f.read()
    trees = parse(data)
    return get_data(trees)

def process_tree(tree):
    processed_tree = {}
    for word in tree:
        processed_tree[word["id"]] = [word["lemma"].lower(), word["head"], word["deprel"]]
    return processed_tree        

def get_data(trees):
    features, labels = [], []
    for tree in trees:
        processed_tree = process_tree(tree)
        for key, value in processed_tree.items():
            word_lem = value[0]
            head_id = value[1]            
            head_lem = processed_tree[head_id][0] if head_id != 0 else ""
            dep = value[2]
            if (word_lem in gensim_model and head_lem in gensim_model):                
                head_vector = gensim_model[head_lem] if head_id != 0 else np.zeros(300)
                features.append(np.append(head_vector, gensim_model[word_lem]))
                labels.append(dep)
    return np.array(features), labels

print("Load Vectors")
gensim_model = KeyedVectors.load_word2vec_format("fiction.lowercased.lemmatized.300d")
print("Load Train Data")
train_features, train_labels = prepare_data('uk_iu-ud-train.conllu')
print("Load Test Data")
test_features, test_labels = prepare_data('uk_iu-ud-test.conllu')

print("Binarize Labels")
labels_dict = {x: 1 for x in train_labels + test_labels}
labels_list = [key for key, value in labels_dict.items()]

train_labels = label_binarize(train_labels, classes=labels_list)
test_labels = label_binarize(test_labels, classes=labels_list)


print("Build Keras Model")
# LSTM network
inputs = Input(shape=(600,1))

x = Bidirectional(LSTM(64, return_sequences=True),
                  merge_mode='concat')(inputs)
x = Dropout(0.2)(x)
x = Flatten()(x)
outputs = Dense(len(labels_list), activation='softmax')(x)

keras_model = Model(inputs=inputs, outputs=outputs, name='LSTM')
train_features = np.expand_dims(train_features, axis=2)
test_features = np.expand_dims(test_features, axis=2)

print("Compile Keras Model")
# Compile the model
sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
keras_model.compile(optimizer=sgd, loss='categorical_crossentropy', metrics=['acc'])

# Define number of epochs
epochs = 15

print("Fit Model")

# Fit the model to the training data
estimator = keras_model.fit(train_features, train_labels, validation_split=0.2, epochs=epochs, batch_size=128, verbose=1)

print("Training accuracy: %.2f%% / Validation accuracy: %.2f%%" % 
      (100*estimator.history['acc'][-1], 100*estimator.history['val_acc'][-1]))

print("Make Prediction")
# Make predictions
predicted_prob = keras_model.predict(test_features)

# Report log loss and score
loss_sk = log_loss(test_labels, predicted_prob)
print('Log loss is: {}'.format(loss_sk))
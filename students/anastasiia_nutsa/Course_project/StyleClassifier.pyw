import numpy as np
import spacy
from collections import OrderedDict
from sklearn.feature_extraction import DictVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn import svm
from sklearn.metrics import classification_report
from sklearn.preprocessing import label_binarize
from sklearn.multiclass import OneVsRestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.preprocessing import Imputer

def get_style_lists(filename, checkList, prefix):
    lst1, lst2, lst3 = [], [], []
    with open(filename, 'r', encoding='utf-8') as f:
        flist = f.readlines()
    for elem in flist:
        ct = elem.count(' ')
        if ct == 2:
            lst3.append(elem.strip().lower())
        elif ct == 1:
            lst2.append(elem.strip().lower())
        else:
            lst1.append(elem.strip().lower())
    checkList[prefix + '1'] = tuple(lst1)
    checkList[prefix + '2'] = tuple(lst2)
    checkList[prefix + '3'] = tuple(lst3)    

def get_formal_informal_count(token, result):
    leftLems = [t.lemma_ for t in token.lefts]
    leftText = [t.text.lower() for t in token.lefts]
    rightLems = [t.lemma_ for t in token.rights]
    rightText = [t.text.lower() for t in token.rights]

    if token.text.startswith("'"):
        if leftText:
            if leftText[-1] + token.text in checkList['i1']:
                result['InformalCount'] += 1
                        
    if token.lemma_.lower() in checkList['i1']:
        result['InformalCount'] += 1

    if token.lemma_.lower() in checkList['f1']:
        result['FormalCount'] += 1

    if leftLems:
        if leftLems[-1] + token.lemma_.lower() in checkList['i2']:
            result['InformalCount'] += 1

        if leftLems[-1] + token.lemma_.lower() in checkList['f2']:
            result['FormalCount'] += 1          

    if rightLems:
        if token.lemma_.lower() + rightLems[0] in checkList['i2']:
            result['InformalCount'] += 1

        if token.lemma_.lower() + rightLems[0] in checkList['f2']:
            result['FormalCount'] += 1

    if leftLems and rightLems:
        if leftLems[-1] + token.lemma_.lower() + rightLems[0] in checkList['i3']:
            result['InformalCount'] += 1

        if leftLems[-1] + token.lemma_.lower() + rightLems[0] in checkList['f3']:
            result['FormalCount'] += 1                 
    

def feature_extraction(sentence):
    result = OrderedDict([('AvgWordLen', 0), ('WordNum', 0), ('IsPassive', 0),
                          ('FormalCount', 0), ('InformalCount', 0), 
                          ('Fpron', 0), ('Ipron', 0)])
    tokens = nlp(sentence)
    length = 0
    cTokens = len(tokens)
    for i in range(0, cTokens):
        token = tokens[i]
        length += len(token)
        if token.dep_.endswith('pass'):
            result['IsPassive'] = 1
        if token.text.lower() in fpron:
            if (i < cTokens - 1 and not tokens[i+1].text.startswith("'")) or i == cTokens - 1:
                result['Fpron'] = 1
        if token.text.lower() in ipron:
            result['Ipron'] = 1
        get_formal_informal_count(token, result)
        
    result['AvgWordLen'] = round(length / cTokens, 1)
    result['WordNum'] = cTokens
    return result

def prepare_data(data):
    processed = 0
    features, labels = [], []
    for style, sent in data:
        features.append(feature_extraction(str(sent)))
        labels.append('informal' if float(style) < 0 else 'formal')
        processed += 1
        if processed % 500 == 0:
            print("Processed:", processed)
    print("Processed:", processed)
    return features, labels
    
def vectorize_data(data, vectorizer, imputer):
    return imputer.transform(vectorizer.transform(data))

#omitPOS =  ('PUNCT', 'SYM', 'NUM', 'PROPN', 'ADP', 'DET', 'CCONJ', 'SCONJ')
nlp = spacy.load('en')
fpron = ('it', 'he', 'she')
ipron = ('i', 'you')
checkList = {}
get_style_lists('formal.txt', checkList, 'f')
get_style_lists('informal.txt', checkList, 'i')
trainSentences = np.genfromtxt('dataTrain.txt', dtype='str', delimiter='\t', encoding='latin1')
testSentences = np.genfromtxt('dataTest.txt', dtype='str', delimiter='\t', encoding='latin1')

train_features, train_labels = prepare_data(trainSentences)
test_features, test_labels = prepare_data(testSentences)

vectorizer = DictVectorizer()
imputer = Imputer()
vec = vectorizer.fit(train_features + test_features)
imp = imputer.fit(vectorizer.transform(train_features + test_features))

train_features_vectorized = vectorize_data(train_features, vec, imp)
test_features_vectorized = vectorize_data(test_features, vec, imp)

##print("SVM Classifier")
##svc = OneVsRestClassifier(svm.LinearSVC(random_state=42))
##svc.fit(train_features_vectorized, train_labels)
##predicted = svc.predict(test_features_vectorized)
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

##print("Naive Bayes Classifier")
##gnb = GaussianNB().fit(train_features_vectorized.toarray(), train_labels)
##predicted = gnb.predict(test_features_vectorized.toarray())
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

##print("K-Nearest Neighbours Classifier")
##knn = KNeighborsClassifier(n_neighbors = 7).fit(train_features_vectorized, train_labels)
##predicted = knn.predict(test_features_vectorized)
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

##print("Descision Tree Classifier")
##dtc = DecisionTreeClassifier(max_depth = 2).fit(train_features_vectorized, train_labels)
##predicted = dtc.predict(test_features_vectorized)
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

print("Logistic Regression Classifier")
lrc = LogisticRegression(random_state=42)
lrc.fit(train_features_vectorized, train_labels)
predicted = lrc.predict(test_features_vectorized)
print(classification_report(test_labels, predicted, digits=3))
print("Feature's weight", lrc.coef_)

correct = 0
for i in range(0, len(predicted)):
    if predicted[i] == test_labels[i]:
        correct += 1
print('Accuracy:', round(correct / len(test_labels), 3))

tmp_sents = ['Nice to see you!', 'Come again!',
             'Hot as hell!', 'You could fry an egg on the sidewalk.',
             'It is extremely hot today.',
             'As the price of five dollars was reasonable, I decided to make the purchase without further thought.',
             'This is to inform you that your book has been rejected by our publishing company as it was not up to the required standard.',
             'The people at the school seemed to be fairly amused by the mishaps in the playground yesterday.']

for sent in tmp_sents:
    print(sent)
    s = feature_extraction(sent)
    sent_features_vectorized = vectorize_data([s], vec, imp)
    print(lrc.predict(sent_features_vectorized)[0])
    print('\n' + 50*'#' + '\n')

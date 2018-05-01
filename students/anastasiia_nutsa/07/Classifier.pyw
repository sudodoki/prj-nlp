import spacy
import json
import numpy as np
from sklearn import linear_model
from sklearn.feature_extraction import DictVectorizer
from sklearn.metrics import classification_report
from sklearn import model_selection
from sklearn.preprocessing import label_binarize 

nlp = spacy.load('en')
vec = DictVectorizer()
data = json.load(open('data.json', 'r', encoding='latin1'))
dataTest = json.load(open('run-on-test.json', 'r', encoding='latin1'))

#extract features for word-1, word, word+1
#word-1: word, lemma, pos, tag, istitle, isdigit, ngram_W_W+1
#word: word, lemma, pos, tag, istitle, isdigit
#word+1: word, lemma, pos, tag, istitle, isdigit, ngram_W-1_W
def WordToFeatures(doc, i):
    result = dict()
    wrd = doc[i]
    wrd0 = doc[i-1] if i != 0 else ""
    wrd1 = doc[i+1] if i != len(doc)-1 else ""

    wrd_text = wrd.text.lower()
    wrd0_text = "NONE" if not wrd0 else wrd0.text.lower()
    wrd1_text = "NONE" if not wrd1 else wrd1.text.lower()
    
    result["word-1"] = wrd0_text
    result["word-1_lemma"] = wrd0.lemma_ if wrd0 else "NONE"
    result["word-1_pos"] = wrd0.pos_ if wrd0 else "NONE"
    result["word-1_tag"] = wrd0.tag_ if wrd0 else "NONE"
    result["word-1_istitle"] = wrd0.text.istitle() if wrd0 else 0
    result["word-1_isdigit"] = wrd0.text.isdigit() if wrd0 else 0
    result["word-1_ngram"] = wrd_text + "_" + wrd1_text

    result["word"] = wrd_text
    result["word_lemma"] = wrd.lemma_
    result["word_pos"] = wrd.pos_
    result["word_tag"] = wrd.tag_
    result["word_istitle"] = wrd.text.istitle()
    result["word_isdigit"] = wrd.text.isdigit()

    result["word+1"] = wrd1_text
    result["word+1_lemma"] = wrd1.lemma_ if wrd1 else "NONE"
    result["word+1_pos"] = wrd1.pos_ if wrd1 else "NONE"
    result["word+1_tag"] = wrd1.tag_ if wrd1 else "NONE"
    result["word+1_istitle"] = wrd1.text.istitle() if wrd1 else 0
    result["word+1_isdigit"] = wrd1.text.isdigit() if wrd1 else 0
    result["word+1_ngram"] = wrd0_text + "_" + wrd_text    
    return result  

def SentenceToFeatures(snt):
    sentence = ' '.join([x[0] for x in snt])
    doc = nlp(sentence)
    lstX = [WordToFeatures(doc, i) for i in range(0, len(doc))]
    lstY = [1 if x[1] else 0 for x in snt]
    if len(lstX) == len(lstY):
        return lstX, lstY
    else:
        return [], []

X_TrainTest = []
Y_TrainTest = []

p = 0

for el in data:
    sent, y = SentenceToFeatures(el)
    p += 1
    if p % 100 == 0:
        print("Processed:", p)
    if sent:
        X_TrainTest += sent
        Y_TrainTest += y
##    if p == 1000:
##        break

dataLen = len(X_TrainTest)
    
for el in dataTest:
    sent, y = SentenceToFeatures(el)
    if sent:
        X_TrainTest += sent
        Y_TrainTest += y

testLen = len(X_TrainTest) - dataLen        

X_TrainTest = vec.fit_transform(X_TrainTest).toarray()

print("Logistic Regression")

#X_train, X_test, Y_train, Y_test = model_selection.train_test_split(X_TrainTest, Y_TrainTest, test_size=0.7, random_state=7)
X_train = X_TrainTest[0:dataLen]
Y_train = Y_TrainTest[0:dataLen]
X_test = X_TrainTest[-testLen:-1]
Y_test = Y_TrainTest[-testLen:-1]

Y_train = label_binarize(Y_train, classes=[0, 1])
Y_test = label_binarize(Y_test, classes=[0, 1])

model = linear_model.LogisticRegression()
model.fit(X_train, Y_train.ravel())
predicted = model.predict(X_test)
report = classification_report(Y_test, predicted)
print(report)

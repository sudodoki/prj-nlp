import numpy as np
import pymorphy2
import re

WORDS_RE = re.compile(r"[^\x00-\x7F]+")
allowedPOS = ['ADJF', 'ADJS', 'COMP', 'VERB', 'INFN', 'PRTF', 'PRTS', 'GRND', 'ADVB']

morph = pymorphy2.MorphAnalyzer(lang='uk')

trainRevs = np.genfromtxt("dataTrain.txt", dtype="str", delimiter = "\t", encoding="utf-8")
testRevs = np.genfromtxt("dataTest.txt", dtype="str", delimiter = "\t", encoding="utf-8")

trainDict = {}
totalDict = {'1': 0, '2': 0, '3': 0, '4': 0, '5': 0}

for rating, review in trainRevs:    
    words = WORDS_RE.findall(review)
    for word in words:
        p = morph.parse(word)[0]
        if p.tag.POS in allowedPOS:
            totalDict[rating] += 1
            if not p.normal_form in trainDict:
                trainDict[p.normal_form] = [0, 0, 0, 0, 0]                
            trainDict[p.normal_form][int(rating) - 1] += 1

for key in trainDict:
    for i in range(0, 5):
        if totalDict[str(i + 1)] > 0:
            trainDict[key][i] = round(trainDict[key][i] / totalDict[str(i + 1)], 5)
            
predicted = 0

for rating, review in testRevs:
    starList = [0, 0, 0, 0, 0]
    words = WORDS_RE.findall(review)
    for word in words:
        p = morph.parse(word)[0]
        if p.tag.POS in allowedPOS:
            key = p.normal_form
            if key in trainDict:
                starList[trainDict[key][::-1].index(max(trainDict[key]))] += 1
    if rating == str(starList[::-1].index(max(starList)) + 1):
        predicted += 1

print("Naive Bayes Precision: {}%".format(str(round(predicted/len(testRevs)*100 , 2))))

def CalcTone(text):
    posTone = 0
    posToneCount = 0
    negTone = 0
    negToneCount = 0
    words = WORDS_RE.findall(text)
    for word in words:
        p = morph.parse(word)[0]
        if p.tag.POS in allowedPOS:
            key = p.normal_form
            if key in toneDict:
                val = float(toneDict[key])
                if val < 0.1:
                    negTone += val
                    negToneCount += 1
                else:
                    posTone += val
                    posToneCount += 1
                    
    if posToneCount + negToneCount == 0:
        return 0

    if posToneCount == 0:
        return negTone / negToneCount

    if negToneCount == 0:
        return posTone / posToneCount
                    
    if negToneCount > posToneCount:
        return negTone / negToneCount
    else:
        return posTone / posToneCount

def evaluate_classifier(X_train, X_test, Y_train, Y_test):
    from sklearn import svm
    from sklearn.preprocessing import label_binarize    

    random_state = np.random.RandomState(0)

    # Use label_binarize to be multi-label like settings
    Y_train = label_binarize(Y_train, classes=[1, 2, 3, 4, 5])
    Y_test = label_binarize(Y_test, classes=[1, 2, 3, 4, 5])
    n_classes = Y_train.shape[1]

    # We use OneVsRestClassifier for multi-label prediction
    from sklearn.multiclass import OneVsRestClassifier

    # Run classifier
    classifier = OneVsRestClassifier(svm.LinearSVC(random_state=random_state))
    classifier.fit(X_train, Y_train)
    y_score = classifier.decision_function(X_test)

    from sklearn.metrics import precision_recall_curve
    from sklearn.metrics import average_precision_score

    # For each class
    precision = dict()
    recall = dict()
    average_precision = dict()
    for i in range(n_classes):
        precision[i], recall[i], _ = precision_recall_curve(Y_test[:, i],
                                                            y_score[:, i])
        average_precision[i] = average_precision_score(Y_test[:, i], y_score[:, i])

    # A "micro-average": quantifying score on all classes jointly
    precision["micro"], recall["micro"], _ = precision_recall_curve(Y_test.ravel(),
        y_score.ravel())
    average_precision["micro"] = average_precision_score(Y_test, y_score,
                                                         average="micro")
    return average_precision["micro"]    

toneData = np.genfromtxt("tone-dict-ukr-auto.tsv", dtype="str", delimiter = "\t", encoding="utf-8")
toneDict = {}

for word, tone in toneData:
    toneDict[word] = tone

X_train = np.array([])
Y_train = np.array([])
X_test = np.array([])
Y_test = np.array([])

for rating, review in trainRevs:
    tone = CalcTone(review)
    if tone > 0:
        X_train = np.append(X_train, [tone])
        Y_train = np.append(Y_train, [int(rating)])

X_train = X_train.reshape(-1, 1)

predicted = 0
total = 0

for rating, review in testRevs:
    tone = CalcTone(review)
    if tone > 0:
        X_test = np.append(X_test, [tone])
        Y_test = np.append(Y_test, [int(rating)])
    else:
        starList = [0, 0, 0, 0, 0]
        total += 1
        words = WORDS_RE.findall(review)
        for word in words:
            p = morph.parse(word)[0]
            if p.tag.POS in allowedPOS:
                key = p.normal_form
                if key in trainDict:
                    starList[trainDict[key][::-1].index(max(trainDict[key]))] += 1
        if rating == str(starList[::-1].index(max(starList)) + 1):
            predicted += 1        
        

X_test = X_test.reshape(-1, 1)        

bowPred = 0

if total > 0:
    bowPred = predicted / total

result = evaluate_classifier(X_train, X_test, Y_train, Y_test)
print("Linear SVC Precision: {}%".format(round((result + bowPred) * 50, 2)))

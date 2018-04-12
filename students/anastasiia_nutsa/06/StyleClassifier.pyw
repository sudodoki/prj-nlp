import numpy as np
import spacy
import re

# 0 - informal, 1 - formal

nlp = spacy.load('en')
stopWords = ('i', 'be', 'have', 'will', 'do')
omitPOS =  ('PUNCT', 'SYM', 'NUM', 'PROPN', 'ADP', 'DET', 'CCONJ', 'SCONJ')

trainSentences = np.genfromtxt('dataTrain.txt', dtype='str', delimiter='\t', encoding='latin1')
testSentences = np.genfromtxt('dataTest.txt', dtype='str', delimiter='\t', encoding='latin1')

trainDict = {}
ct = 0
processed = 0

#Get words frequences
for style, sentence in trainSentences:
    sent = nlp(str(sentence))
    index = 0 if float(style) < 0 else 1
    processed += 1
    if processed % 500 == 0:
        print("Processed: {}".format(str(processed)))
    for i in range(0, len(sent)):
        if not sent[i].pos_ in omitPOS:
            token = sent[i].text            
            if ("'" in token) and (len(token) <= 3) and (i > 0):
                token = sent[i-1].text + token
            else:
                token = sent[i].lemma_
                if token.lower() in stopWords:
                    token = ""
            if token:        
                if not token in trainDict:
                    trainDict[token.lower()] = [0, 0]
                trainDict[token.lower()][index] += 1
                ct += 1
print('Words amount: {}'.format(ct))
print('DictSize1: {}'.format(len(trainDict)))

#Remove words with single occurance
clearDict = []
for key in trainDict:
    if (trainDict[key][0] + trainDict[key][1]) < 3:
        clearDict.append(key)
for key in clearDict:
    del trainDict[key]
print('DictSize2: {}'.format(len(trainDict)))

#Remove words with difference in distribution less than 0.3%
clearDict = []
for key in trainDict:
    dif = abs(trainDict[key][0] - trainDict[key][1]) / (trainDict[key][0] + trainDict[key][1])
    if dif < 0.3:
        clearDict.append(key)
for key in clearDict:
    del trainDict[key]
print('DictSize3: {}'.format(len(trainDict)))

#Sort words by most frequent
trainList = []
for key in trainDict:
    temp = [key, trainDict[key][0], trainDict[key][1]]
    trainList.append(temp)
trainList = sorted(trainList, key=lambda x: x[1] + x[2], reverse=True)
#Write sorted dictionary to the file
testOutput = open('testOutput.txt', 'w', encoding='latin1')
for elem in trainList:
    testOutput.write('{}\t{}\t{}\n'.format(str(elem[1]), str(elem[2]), elem[0]))
testOutput.close()

#Run BoW for testData variables: ct, style - for each sentence; predicted - for the whole set
predicted = 0
processed = 0
formalTotal = 0
formalFound = 0
informalFound = 0

for style, sentence in testSentences:
    sent = nlp(str(sentence))
    index = 0 if float(style) < 0 else 1
    if index == 1:
        formalTotal += 1
    processed += 1
    tokenSum = 0
    formalSum = 0
    sentStyle = 0
    informalSum = 0
    if processed % 500 == 0:
        print("Processed: {}".format(str(processed)))
    for i in range(0, len(sent)):
        if not sent[i].pos_ in omitPOS:
            token = sent[i].text            
            if ("'" in token) and (len(token) <= 3) and (i > 0):
                token = sent[i-1].text + token
            else:
                token = sent[i].lemma_
                if token.lower() in stopWords:
                    token = ""
            token = token.lower()        
            if token:
                if token in trainDict:
##                    tokenSum += 1
##                    sentStyle += trainDict[token][1] / (trainDict[token][0] + trainDict[token][1])
##    if tokenSum > 0:
##        sentStyle = sentStyle / tokenSum
##    sentStyle = 0 if sentStyle < 0.5 else 1
                    if trainDict[token][1] > trainDict[token][0]:
                        formalSum += 1
                    else:
                        informalSum += 1            
    sentStyle = 0 if informalSum >= formalSum else 1                    
    if sentStyle == index:
        predicted += 1
        if index == 1:
            formalFound += 1
        else:
            informalFound += 1
print('fFound:\t{}\nfTotal:\t{}\ninfFound:\t{}\ninfTotal:\t{}\nPredicted:\t{}\nProcessed:\t{}'.format(
    formalFound, formalTotal, informalFound, (processed - formalTotal), predicted, processed))

formalRecall = formalFound / formalTotal
informalRecall = informalFound / (processed - formalTotal)
precision = predicted / processed
avgRecall = (formalRecall + informalRecall) / 2
f = 2 * (precision * avgRecall) / (precision + avgRecall)
print('Formal recall: {}'.format(str(formalRecall)))
print('Informal recall: {}'.format(str(informalRecall)))
print('Precision: {}'.format(precision))
print('F: {}'.format(f)) 

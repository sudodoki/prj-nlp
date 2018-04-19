import spacy
import json
import numpy as np

nlp = spacy.load('en')
data = np.genfromtxt('data.txt', dtype='str', delimiter='\n', encoding='utf-8')

runOnSents = []

def sentMark(s, last):
    result = []
    punct = not last
    for i in range(len(s) - 1, -1, -1):
        token = s[i]
        if not punct:
           result = [[token.text, False]] + result           
        if punct:
            if token.pos_ != 'PUNCT':
                punct = False
                result = [[token.text, True]] + result           
    return result

def processSentences(s1, s2, s3):
    tmp = []
    tmp += sentMark(s1, False)
    tmp += sentMark(s2, not s3)
    if s3:
        tmp += sentMark(s3, True)
    runOnSents.append(tmp)

for el in data:
    i = 0
    lst = []
    doc = nlp(str(el))
    for sentence in doc.sents:
        lst.append(sentence)    
    if len(lst) > 1:
        if len(lst) % 2:
            processSentences(lst[0], lst[1], lst[2])
            i += 3
        while i < len(lst):
            processSentences(lst[i], lst[i + 1], None)
            i += 2
    ln = len(runOnSents)       
    if (ln % 500 < 10):
        print("Processed: {}".format(ln))
    if (ln > 10000):
        break
        

with open('data.json', 'w', encoding='utf-8') as f:
    json.dump(runOnSents, f)

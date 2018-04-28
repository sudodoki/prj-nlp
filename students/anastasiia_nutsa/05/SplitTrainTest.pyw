import numpy as np
import math

reviews = np.genfromtxt("dataUkr.txt", dtype="str", delimiter = "\t", encoding="utf-8")

processed = 0

revDict = {}

for rating, review in reviews:
    processed += 1
    if processed % 100 == 0:
        print("Processed: " + str(processed))

    if rating in revDict:
        revDict[rating].append(review)
    else:
        revDict[rating] = [review]

starLens = [len(revDict[x]) for x in revDict]
testCount = int(math.ceil((min(starLens) * 0.3)))

testDict = {}
trainDict = {}
for rating in revDict:
    trainDict[rating] = revDict[rating][0:len(revDict[rating]) - testCount - 1]
    testDict[rating] = revDict[rating][-1 * testCount - 1:-1]

trainFile = open('dataTrain.txt', 'w', encoding='utf8')
testFile = open('dataTest.txt', 'w', encoding='utf8')

for rating in trainDict:
    for review in trainDict[rating]:
        trainFile.write("{}\t{}\n".format(rating, review))

for rating in testDict:
    for review in testDict[rating]:
        testFile.write("{}\t{}\n".format(rating, review))                

trainFile.close()
testFile.close()

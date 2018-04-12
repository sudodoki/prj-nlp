import numpy as np
import math
import sys


dataFiles = ['answers', 'blog', 'news', 'email']

trainFile = open('dataTrain.txt', 'w', encoding='latin1')
testFile = open('dataTest.txt', 'w', encoding='latin1')

dataSet = {}

#Read data from files
for file in dataFiles:
    sentences = np.genfromtxt(file, dtype='str', delimiter='\t',
                              usecols=(0, -1), encoding='latin1')
    dataSet[file] = sentences

#Remove sentences with neutral style
styledDataSet = {}
for key in dataSet:
    styledDataSet[key] = []
    for style, sentence in dataSet[key]:
        if abs(float(style)) >= 0.8 and sentence:
            roundedStyle = str(round(float(style), 2))
            styledDataSet[key].append([roundedStyle, sentence])

#Split data into train and test 70:30
setLens = [len(styledDataSet[x]) for x in styledDataSet]
testCount = int(math.ceil(min(setLens) * 0.3))
print('TotalCount: {}'.format(setLens))
print('TestCount = {}'.format(str(testCount)))

#Fill train and test files with sentences
for key in styledDataSet:
    lst = styledDataSet[key]
    for i in range(0, len(lst) - testCount):
        trainFile.write('{}\t{}\n'.format(lst[i][0], lst[i][1]))
    for i in range(len(lst) - testCount - 1, len(lst)):
        testFile.write('{}\t{}\n'.format(lst[i][0], lst[i][1]))

trainFile.close()
testFile.close()

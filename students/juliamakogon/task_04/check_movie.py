import spacy
import glob
import os


def loadSpacy():
    nlp = spacy.load('en')
    print("SpaCy en model loaded")
    return nlp



class CheckMovieFairytale:
    def __init__(self, nlp):
        self.nlp = nlp
        self.db = {}


    def load(self, databaseFileName):
        with open(databaseFileName, 'r', encoding='utf-8') as f:
            line = f.readline()
            while line:
                items = line.split('\t')
                movie = items[0]
                item_ar = []
                if movie in self.db:
                   item_ar = self.db[movie]
                item_ar.append(items)
                self.db[movie] = item_ar
                line = f.readline()


    def check(self, text):
        return None


    def checkDirectory(self, path):
        positive = 0
        negative = 0
        for fname in glob.glob(os.path.join(path, '*.txt')):
            print(fname)
            with open(fname, 'r', encoding='utf-8') as f:
                result = self.check(f.readlines)
                if result:
                    positive += 1
                else:
                    negative += 1
        return (positive, negative)        


def calcStats(true_pos, false_pos, false_neg, message = ''):
    def ds(d): return f'{d:.5f}'
    precision = 0
    recall = 0
    f1 = 0
    if true_pos > 0: 
        precision = true_pos/(true_pos + false_pos)
        recall = true_pos/(false_neg + true_pos)
        f1 = 2 * precision * recall / (precision + recall) 
    print("{}\tf1\t{}\tprecision\t{}\trecall\t{}".format(message, ds(f1), ds(precision), ds(recall)))



testpath = r'C:\work\jul\prj-nlp\students\juliamakogon\task_04\testset'
database = 'fairytale_db.csv'
pos_path = os.path.join(testpath, r'pos')
neg_path = os.path.join(testpath, r'neg')

nlp = loadSpacy()
checker = CheckMovieFairytale(nlp)
checker.load(database)

(true_pos, false_neg) = checker.checkDirectory(pos_path)
(false_pos, true_neg) = checker.checkDirectory(neg_path)
print("{}\t{}\n{}\t{}".format('true+', 'false-', 'false+', 'true-'))
print("{}\t{}\n{}\t{}".format(true_pos, false_neg, false_pos, true_neg))
calcStats(true_pos+true_neg, false_pos, false_neg, "Statistics for the classifier:\t")
calcStats(true_pos, false_pos, false_neg, "Statistics for the 'positive' category:")
calcStats(true_neg, false_neg, false_pos, "Statistics for the 'negative' category:")





import spacy
import glob
import os
import re


def loadSpacy():
    nlp = spacy.load('en')
    print("SpaCy en model loaded")
    return nlp

class CheckMovieFairytale:
    def __init__(self, nlp):
        self.nlp = nlp
        self.db = {}
        self.verb_lemmas = []
        for w in ['inspired', 'based']:
            self.verb_lemmas.append(self.nlp(w)[0].lemma_)

    def load(self, databaseFileName):
        with open(databaseFileName, 'r', encoding='utf-8') as f:
            line = f.readline()
            while line:
                items = line.split('\t')
                movie = items[0].strip()
                item_ar = []
                if movie in self.db:
                   item_ar = self.db[movie]
                item_ar.append(items[1].strip())
                self.db[movie] = item_ar
                line = f.readline()
        print(databaseFileName)


    def extractName_NNP_is_movie(self, doc):
        for t in doc:
            if t.lemma_ in ['movie', 'film', 'sequel']:
                i = t.i
                while i > 0 and doc[i].lemma_ != 'be' and not doc[i-1].is_space and not re.search(r"([\'\w]*)[\.]", doc[i-1].text):
                    i -= 1       
                root = t.doc[i] # formally it's not a root, but there are bugs on some texts          
                if root.lemma_ == 'be': #extracting pattern 'NNP is a movie'
                    if root.n_lefts > 0:
                        start = root.i - 1
                        while start > 0 and not t.doc[start-1].is_space and not re.search(r"([\'\w]*)[\.]", t.doc[start-1].text):
                            start -= 1
                        title = t.doc[start : root.i].text
                        if title.startswith("This article is about"):
                            continue
                        match = re.findall(r"The movie (.+)", title, re.I)
                        if match:
                            title = match[0]
                        match = re.findall(r"(.+)\(", title)
                        if match:
                            title = match[0].strip()                     
                        return title
        return None           

    def extractYear_is_yyyy_movie(self, doc):
        for t in doc:
            if t.lemma_ == 'movie' or t.lemma_ == 'film':
                for tt in t.subtree:             
                    if re.search(r"\d{4}", tt.text):
                        return tt.text
        return None


    def extractBased_film_based_on(self, doc):
        for t in doc:
            if t.pos_ == 'VERB' and t.lemma_ in self.verb_lemmas:
                based_on = []
                prep_a = [x for x in t.children if x.dep_ == 'prep' or x.pos_ == 'ADD']
                for prep in prep_a:
                    quotes = [x for x in prep.subtree if x.text == '\"']
                    if len(quotes) == 2:
                        based_on.append(t.doc.text[quotes[0].idx + 1 : quotes[1].idx]) # in the quotes it's a story name
                    pobj = [x for x in prep.subtree if x.dep_ == 'pobj']
                    for p in pobj:
                        b = [x.text for x in p.subtree]
                        ss = []
                        for bb in b:
                            match = re.findall(r"(?=\D)([\'\w]+)", bb)
                            if match:
                                ss.extend(match)
                        s = ' '.join(ss)
                        return s
        return None

    def extractBased_Cinderella(self, doc):
        for t in doc:
            if t.text == 'Cinderella':
                return t.text
        return None        

        

    def extract(self, text):
        doc = self.nlp(text)
        movie = self.extractName_NNP_is_movie(doc)
        year = self.extractYear_is_yyyy_movie(doc)
        base_story = self.extractBased_Cinderella(doc)
        if not base_story:
            base_story = self.extractBased_film_based_on(doc)
        return (movie, year, base_story)


    def check(self, text, check_year = True):
        (movie, year, based_on) = self.extract(text)
        pos = False
        if based_on and re.search('Cinderella', based_on):
            if movie in self.db:
                if check_year:
                    recs = self.db[movie]
                    if not year:
                        year = '0'
                    for r in recs:
                        if r == '0' or year == r:
                            pos = True
                else:
                    pos = True
        result = (pos, movie, year, based_on)
        return result


    def checkDirectory(self, path, print_only_neg = False, check_year = True):
        positive = 0
        negative = 0
        for fname in glob.glob(os.path.join(path, '*.txt')):
            with open(fname, 'r', encoding='utf-8') as f:
                result = self.check('\n'.join(f.readlines()), check_year)
                if result[0]:
                    positive += 1
                else:
                    negative += 1
                if (print_only_neg and not result[0]) or (not print_only_neg):
                    print(fname)
                    print(result)
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


if __name__ == '__main__':
    dirpath = os.getcwd()
    testpath = os.path.join(dirpath, r'testset')
    database = 'cinderella_db.csv'
    pos_path = os.path.join(testpath, r'pos')
    neg_path = os.path.join(testpath, r'neg')

    nlp = loadSpacy()
    checker = CheckMovieFairytale(nlp)
    checker.load(database)

    for check_year in [True, False]:
        print("-"*30)
        (true_pos, false_neg) = checker.checkDirectory(pos_path, print_only_neg = True, check_year=check_year)
        (false_pos, true_neg) = checker.checkDirectory(neg_path)
        print("{}\t{}\n{}\t{}".format('true+', 'false-', 'false+', 'true-'))
        print("{}\t{}\n{}\t{}".format(true_pos, false_neg, false_pos, true_neg))
        calcStats(true_pos+true_neg, false_pos, false_neg, "Statistics for the classifier:\t")
        calcStats(true_pos, false_pos, false_neg, "Statistics for the 'positive' category:")
        calcStats(true_neg, false_neg, false_pos, "Statistics for the 'negative' category:")





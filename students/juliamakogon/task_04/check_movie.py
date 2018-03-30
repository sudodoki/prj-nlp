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
                for i in range(len(items)):
                    match = re.findall(r"^\"(.+)\"", items[i])
                    if match:
                        items[i] = match[0]
                    if i == 2:
                        match = re.findall(r'works by (.*)', items[i])
                        if match:
                            items[i] = match[0]
                        if items[i] == 'Grimms\' Fairy Tales':
                            items[i] = 'Grimms'
                movie = items[0]
                item_ar = []
                if movie in self.db:
                   item_ar = self.db[movie]
                item_ar.append(items)
                self.db[movie] = item_ar
                line = f.readline()
        print(databaseFileName)


    def extractName_NNP_is_movie(self, doc):
        for t in doc:
            if t.lemma_ == 'movie' or t.lemma_ == 'film':
                root_a = [x for x in t.ancestors if x.dep_ == 'ROOT']
                if len(root_a) == 0: # just in case
                    continue
                root = root_a[0]
                if root.lemma_ == 'be': #extracting pattern 'NNP is a yyyy movie'
                    if root.n_lefts > 0:
                        start = root.i - 1
                        while start >= 0 and not re.search(r"([\'\w]*)[\.,]", t.doc[start].text):
                            start -= 1
                        start = max(start, 0)
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

    def extractName(self, t):
        movie = None
        if t.lemma_ == 'movie' or t.lemma_ == 'film':
            movie = []
            root_a = [x for x in t.ancestors if x.dep_ == 'ROOT']
            if len(root_a) == 0: # just in case
                return None
            root = root_a[0]
            if root.lemma_ == 'be': #extracting pattern 'NNP is a yyyy movie'
                if root.n_lefts > 0:
                    # lf = next(sroot.left)
                    # start = lf.i
                    start = root.i - 1
                    while start >= 0 and not re.search(r"([\'\w]*)[\.,]", t.doc[start].text):
                        start -= 1
                    start = max(start, 0)
                    title = t.doc[start : root.i].text
                    if title.startswith("This article is about"):
                        return None # wiki disambiguation
                    match = re.findall(r"The movie (.+)", title, re.I)
                    if match:
                        title = match[0]
                    match = re.findall(r"(.+)\(", title)
                    if match:
                        title = match[0].strip()                     
                    movie.append(title)
            


        return movie

    def extractYear(self, t):
        year = None
        if t.lemma_ == 'movie' or t.lemma_ == 'film':
            root_a = [x for x in t.ancestors if x.dep_ == 'ROOT']
            if len(root_a) == 0: # just in case
                return None
            root = root_a[0]
            if root.lemma_ == 'be': #extracting pattern 'NNP is a yyyy movie'
                for tt in t.subtree:             
                    if tt.ent_type_ == 'DATE':
                        year = tt.text
        return year


    def extractBasedOn(self, t):
        based_on = None
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
                    based_on.append(s) 
        if based_on and len(based_on) > 0:
            return based_on
        return based_on

        

    def extract(self, text):
        doc = self.nlp(text)
        movie_names = []
        based_on = []
        years = []
        result = ['', '', '']
        # is it a movie? 
        for t in doc:
            # get the movie name and year
            if len(movie_names) == 0:
                movie = self.extractName(t)
                if movie:
                    movie_names.extend(movie)
            if len(years) == 0:
                year = self.extractYear(t)
                if year:
                    years.append(year)                
            base_story = self.extractBasedOn(t)
            if base_story:
                based_on.extend(base_story) 

        # for m in movie_names:
        #     print(m)
        # for y in years:
        #     print(y)
        # for b in based_on:
        #     print(b)

        if len(movie_names) > 0:
            result[0] = movie_names[0]
        if len(years) > 0:
            result[1] = years[0]
        if len(based_on) > 0:
            result[2] = based_on[0]
        return result


    def check(self, text):

        result = self.extract(text)
        movie = result[0]
        year = result[1]
        based_on = result[2]
        print(movie, year, based_on)
        if movie in self.db:
            recs = self.db[movie]
            if not year:
                year = '0'
            if len(recs) > 1:
                recs = [x for x in recs if x[1] == year]
            for r in recs:
                if r[1] != '0' and year != r[1]:
                    print('year: {} vs in db {}', year, r[1])
                    return None
            for r in recs:
                if based_on.lower().find(r[2].lower()) >= 0:
#                if re.search(based_on, r[2]) or re.search(r[2], based_on):
                    return result
        return None


    def checkDirectory(self, path):
        positive = 0
        negative = 0
        for fname in glob.glob(os.path.join(path, '*.txt')):
            print(fname)
            with open(fname, 'r', encoding='utf-8') as f:
                result = self.check('\n'.join(f.readlines()))
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


if __name__ == '__main__':
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





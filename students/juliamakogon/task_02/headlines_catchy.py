import nltk
import sys
from nltk.corpus import sentiwordnet as swn
from nltk.stem import WordNetLemmatizer

class HeadlinerNLTK(): 
    lemmatizer = None;
    def __init__(self):
            self.lemmatizer = WordNetLemmatizer()
            print("WordNetLemmatizer loaded.")
    
    def catchHeader(self, s):
        doc = nltk.word_tokenize(s)
        doc[0] = doc[0].lower() #otherwise it'll be named entity in nltk
        pos_tags = nltk.pos_tag(doc)
        ner_chunks = nltk.ne_chunk(pos_tags, binary=True)
        prominence = False; 
        superlativeness = False
        sentiment = False
#        supertags = ["JJS", "JJR", "RBS", "RBR"]
        supertags = ["JJS", "RBS"]
        sentitag = {'NN':'n', 'NNS':'n', 'NNP':'n', 'NNPS':'n', 
                    'JJ':'a', 'JJR':'a', 'JJS':'a',
                    'RB':'r', 'RBR':'r', 'RBS':'r', 
                    'VB':'v', 'VBD':'v', 'VBG':'v', 'VBN':'v', 'VBP':'v', 'VBZ':'v' }
        for t in ner_chunks.subtrees():
            if t.label() == 'NE':
                print(t)
                prominence = True
                break
        for (w, pos) in pos_tags:
            if pos in supertags:
                print(w, pos)
                superlativeness = True
                break
        for (w, pos) in pos_tags:
            if pos in sentitag:
                spos = sentitag[pos]
                lemma = self.lemmatizer.lemmatize(w)
                #print(lemma)
                synset = swn.senti_synsets(lemma, spos)
                k = 0
                avg = 0
                for syn in synset:
                    #print(syn)
                    avg = avg + max(syn.pos_score(), syn.neg_score())
                    k = k + 1
                    if k > 5 :
                        break
                if k > 0:
                    if avg / k > 0.5:
                       print(avg/k, w, lemma)
                       sentiment = True
                       break
            
        return (prominence, superlativeness, sentiment)
    

if __name__ == '__main__':
        h = HeadlinerNLTK()

        inputfile = "examiner-headlines.txt"
        outputfile = "catchy-headlines.txt"
        if len(sys.argv) >= 2:
            inputfile = sys.argv[1]
            print("Using input filename:", inputfile)
        else:
            print("Using default input filename:", inputfile)
        if len(sys.argv) >= 3:
            outputfile = sys.argv[2]
            print("Using output filename:", outputfile)
        else:
            print("Using default output filename:", outputfile)
        n = 0
        k = 0
        with open(inputfile, 'r', encoding = "utf-8") as f:
            with open(outputfile, 'w', encoding = "utf-8") as f2:
                for line in f:
                    if line:
                        n = n + 1
                        (prom, spr, sen) = h.catchHeader(line)
                        if prom or spr or sen:
                            print(line[:-1])
                            print("prominence: {}, superlativeness: {}, sentiment: {}".format(prom, spr, sen))
                            print('---')
                            f2.write(line)
                            k = k + 1
                            
        print("Headlines:\t\t{0}\nCatchy headlines:\t{1}".format(n, k))


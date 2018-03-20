import spacy
import sys

class SpacyInit():
    nlp = None;
    def __init__(self):
        if not SpacyInit.nlp:
            SpacyInit.nlp = spacy.load('en')
            print("Model loaded.")
            
            
class VerbSearcher(SpacyInit):
    
    nlp = None
    counter = None
    
    def loadSpacy():
        nlp = spacy.load('en')
        print("SpaCy en model loaded")
        return nlp
    
    
    def __init__(self, nlp, verbs):
        self.nlp = nlp
        self.verbs = verbs
        self.counter = {}
    
    def search(self, token):
        deps = set()
        if token.pos_ == 'VERB' and token.lemma_ in self.verbs:
            for dep in token.subtree:
                if dep.pos_=='ADV' and not dep.ent_type_ and len(dep.lemma_) > 2 and dep.lemma_[-2:]=='ly':
                    n = 1;
                    if dep.lemma_ in self.counter:
                        n = n + self.counter[dep.lemma_]
                    deps.add(dep.lemma_)
                    self.counter[dep.lemma_] = n
        return deps
    
    def stats(self):
        st = sorted(self.counter.items(), key=lambda t: t[1], reverse = True)
        p = [str(t) for t in st[:10]]
        s = "{}: {}".format(self.verbs[0], " ".join(p))
        return s
    
    
def write(f, s):
    print(s)
    f.write(s+'\n')
    
    
def search(nlp, line, vs):
    if line:
        doc = nlp(line)
        for token in doc:
            for v in vs:
                v.search(token)
                
    
if __name__ == '__main__':
    inputfile =  "blog2008.txt" #"short_blog.txt" #
    outputfile = "saytell.txt"
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
    nlp = VerbSearcher.loadSpacy()
    verbs = set()
    verbs.update(set(['say', 'announce', 'answer', 'assert', 'claim', 'convey', 'declare', 'disclose', 'express', 'mention', 'read', 'repeat', 'reply', 'report', 'respond', 'reveal', 'speak', 'state', 'suggest', 'tell', 'voice']))
    verbs.update(set(['tell', 'advise', 'announce', 'confess', 'declare', 'disclose', 'explain', 'express', 'inform', 'instruct', 'mention', 'notify', 'report', 'reveal', 'say', 'speak', 'state']))
    verbs.update(set(['speak', 'chat', 'communicate', 'convey', 'declare', 'express', 'say', 'shout', 'tell', 'utter', 'voice', 'whisper']))
    verbs.update(set(['claim', 'allege', 'ask', 'assert','call', 'declare', 'acknowledge', 'argue', 'claim', 'explain', 'plead', 'respond', 'beg', 'request', 'yell', 'declare']))
    verbs.update(set(['communicate', 'inform', 'publicize', 'suggest', 'tell', 'write', 'impart']))
    vs = []
    for verb in verbs:
        vs.append(VerbSearcher(nlp,[verb]))

    linenum = 0
    text = ""
    with open(inputfile, 'r', encoding = "utf-8") as f:
        for line in f:
            text = text + line
            linenum = linenum + 1
            if linenum % 100 == 0:
                print(linenum)
                search(nlp, text, vs)
                text = ''
    search(nlp, text, vs)

    with open(outputfile, 'w', encoding = "utf-8") as f2:
        for v in vs:
            if len(v.counter):
                write(f2, v.stats())

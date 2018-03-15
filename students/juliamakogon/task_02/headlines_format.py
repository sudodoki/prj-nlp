import spacy
import sys

class SpacyInit():
    nlp = None;
    def __init__(self):
        if not self.nlp:
            self.nlp = spacy.load('en')
            print("Model loaded.")

class Headliner(SpacyInit):    
    
    def IsToUpper(token, lower_token):
        # Capitalize nouns, pronouns, adjectives, verbs, adverbs, and 
        # subordinate conjunctions    
        pos = ["NOUN", "PROPN", "ADJ", "VERB", "ADV", "PRON"]
        #articles, coordinating conjunctions, prepositions, particles, interjections.
        pos_lower = ["DET", "ADP", "CCONJ", "PART", "INTJ"]
        token_pos = token.pos_
        if (lower_token.pos_ in pos_lower):
            if lower_token.pos_ == "ADP":
                if lower_token.n_lefts + lower_token.n_rights > 0:
                    token_pos = lower_token.pos_
            else:    
                token_pos = lower_token.pos_
        r = token.i == 0 or (token.is_sent_start == True and lower_token.is_sent_start == True)
        if not r:
            r = token_pos in pos

        if not r :
            r = Headliner.IsHyphenated(token)
        if not r:
            r = Headliner.IsLastWord(lower_token)
        return r  
    
    def IsHyphenated(token):
        if token.i > 1:
            text = token.doc.text
            return text[token.idx-1] == '-' and text[token.idx-2].isalpha
        return False
    
    def IsLastWord(token):
        n = len(token.doc)
        i = token.i+1
        while i < n and token.doc[i].is_punct == True:
            i = i+1
        return i >= n or token.doc[i].is_sent_start == True or token.doc[i].is_space == True
    
   
    def IsCaps(token, r):
        return len(token) > 1 and r[token.idx].isupper() and r[token.idx+1].isupper()
    
    def Format(self, s):
        doc_lower = self.nlp(s.lower()) # with this line determiners were recognized better
        doc = self.nlp(s)
        
        r = list(s)
        first = True
        for token in doc:
            i_low = 0
            while doc_lower[i_low].idx < token.idx:
                 i_low = i_low + 1   
#                 
#            print(token.text, token.pos_, doc_lower[i_low].text, doc_lower[i_low].pos_, 
#                                      token.n_lefts + token.n_rights,
#                                      doc_lower[i_low].n_lefts + doc_lower[i_low].n_rights)  
#                
            if token.text.lower() == 'n\'t' or token.text.lower() == '\'s' or token.like_url or token.like_email:
                continue
        
            if token.text.isalpha() or (token.text[-1] == '.' and token.text[:-1].isalpha()):
                if first or Headliner.IsToUpper(token, doc_lower[i_low]):
                    r[token.idx] = r[token.idx].upper()
                elif not Headliner.IsCaps(token, r) and token.pos_ != "PROPN" and token.pos_ != "NOUN":
                    r[token.idx] = r[token.idx].lower()
                first = False
        return ''.join(r);
    

if __name__ == '__main__':
    inputfile = "examiner-headlines.txt"
    outputfile = "corrected-headlines.txt"
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
    h = Headliner();
    n = 0
    k = 0
    with open(inputfile, 'r', encoding = "utf-8") as f:
        with open(outputfile, 'w', encoding = "utf-8") as f2:
            for line in f:
                if line:
                    n = n + 1
                    s = h.Format(line)
                    f2.write(s)
                    if s == line:
                        k = k + 1
    print("Headlines:\t\t{0}\nCorrect headlines:\t{1}".format(n, k))


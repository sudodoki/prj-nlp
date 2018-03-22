import spacy
from nltk.corpus import sentiwordnet as swn


nlp = spacy.load('en')
input = 'examiner-headlines.txt'
output = 'examiner-headlines_res2.txt'
enteties = ['PERSON', 'ORG', 'FACILITY', 'PRODUCT', 'EVENT', 'LAW', 'PERCENT', 'MONEY', 'QUANTITY', 'ORDINAL', 'CARDINAL']
super_tags = ['JJR', 'JJS', 'RBR', 'RBS']
tag_dict = {'NN': 'n', 'NNS': 'n', 'NNP': 'n', 'NNPS': 'n',
                    'JJ': 'a', 'JJR': 'a', 'JJS': 'a',
                    'RB': 'r', 'RBR': 'r', 'RBS': 'r',
                    'VB': 'v', 'VBD': 'v', 'VBG': 'v', 'VBN': 'v', 'VBP': 'v', 'VBZ': 'v'}


def prominent(doc):
    for ent in [ent.label_ for ent in doc.ents]:
        #print(ent)
        if ent in enteties:
            return True
    return False

def superlative(doc):
    for token in doc:
        if token.tag_ in super_tags:
            return True
    return False


def charged(doc):
    pos = []
    for token in doc:
        if token.pos_ in tag_dict.keys(): pos.append((token.lemma_, tag_dict[token.pos_]))
    with_sent = []
    for lemma, tag in pos:
        for s in swn.senti_synsets(lemma):
            with_sent1 = []
            if tag in s.unicode_repr(): with_sent1.append(s)
        try:
            p_score = 0
            n_score = 0
            for s in with_sent1:
                p_score += s.pos_score()/ len(with_sent1)
                n_score += s.neg_score()/ len(with_sent1)
            with_sent.append((p_score, n_score))
        except:
            continue
    for p, n in with_sent:
        if p > 0.5 or n> 0.5:
            return True
    return False

n = 0
m= 0
with open(output, 'w', encoding="utf-8") as f2:
    with open(input, 'r', encoding="utf-8") as f:
        for line in f:
            m += 1
            doc4 = nlp(line)
            prom = prominent(doc4)
            super = superlative(doc4)
            char = charged(doc4)
            if prom == True or super == True or char ==True:
                f2.write(line)
                print(line)
                n+=1
    f.close()
f2.close()
print('All: {}, catchy:{}; '.format(m,n))
# All: 5000, catchy:3358;





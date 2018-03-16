# coding: utf-8

task_dir = '../../../../tasks/02-structural-linguistics/'

with open(task_dir + 'examiner-headlines.txt', 'r') as f:
    headers = f.readlines()
    headers = [h.strip() for h in headers]

# II Catch catchy headlines
import spacy
from nltk.corpus import sentiwordnet as swn
from nltk.corpus import wordnet as wn

nlp = spacy.load('en_core_web_md')

def senti_pos(pos):
    if (pos == 'VERB'):
        return wn.VERB
    if (pos == 'NOUN'):
        return wn.NOUN
    if (pos == 'ADJ'):
        return wn.ADJ
    if (pos == 'ADV'):
        return wn.ADV
    return None

def get_avg_sent(word, pos = None):
    synset = list(swn.senti_synsets(word, senti_pos(pos)))[0:5]
    count = len(synset)
    if (count == 0):
        return 0
    total_pos = 0
    total_neg = 0
    for syn in synset:
        total_pos += syn.pos_score()
        total_neg += syn.neg_score()
    return (total_pos - total_neg) / count

def is_prominent(headline):
    doc = nlp(headline);
    tks = [token for token in doc]
    sent = 0
    for token in tks:
        # should be NER instead but I didn't find trained NER model
        if token.pos_ == 'PROPN':
            return True
        if token.tag_ in ['JJS', 'RBS']:
            return True
        sent += get_avg_sent(str(token), token.pos_)
    if (sent / len(tks)) > 0.5:
        return True
    return False

with open('catchy_output.txt', 'w') as out:
    for  header in headers:
        if (is_prominent(header)):
            out.write(header)
            out.write("\n")
print('Done writing in catchy_output.txt')


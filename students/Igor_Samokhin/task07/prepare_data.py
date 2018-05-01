import pandas as pd
import random
from tqdm import tqdm
from nltk.tokenize import sent_tokenize
import json

import spacy
nlp = spacy.load('en_core_web_md')

# first, download our data
# original source is https://www.kaggle.com/rounakbanik/ted-talks

PATH = '/mnt/hdd/Data/NLP/'
ted = pd.read_csv(PATH+'transcripts.csv')

# little cleaning
ted['transcript'] = ted['transcript'].str.replace('\(.*?\)', ' ').str.strip()

def glue_sents(sents):
    """
    sents is a list of sentences; function returns 
    list of tokens together with boolean for last
    word in a sentence (except for the last sentence)
    """
    glued_tokens = []
    for j, sent in enumerate(sents):
        doc = nlp(sent, disable=['parser', 'ner'])
        if len([t for t in doc if not t.is_punct]) == 0:
            continue
        for token in doc:
            if token.i == 0 and j != 0:
                # randomly uncapitalize some previously capitalized words
                if (random.random() < 0.5
                    and token.pos_ != 'PROPN'
                    and token.text != 'I'):
                    glued_tokens.append([token.text.lower(), False])
                else:
                    glued_tokens.append([token.text, False])
            elif (token.i == [t.i for t in doc 
                              if not t.is_punct][-1]
                  and j != (len(sents)-1)):
                glued_tokens.append([token.text, True])
            elif (j != (len(sents)-1) 
                  and token.is_punct
                  and token.i == (len(doc)-1)):
                continue
            else:
                glued_tokens.append([token.text, False])
    return glued_tokens

def glue_text(text):
    """
    Glue sentences in text in chunks of sizes 3, 4 and 5.
    Originally I used sizes 1,2, and 3, but it turns out 
    it works better with bigger run-on sentences in training data
    """
    sentences = sent_tokenize(text)
    text_len = len(sentences)
    current_ind = 0
    glued_tokens = []
    while current_ind < text_len:
        # choose randomly size of glued sentence
        to_glue_ind = current_ind + random.choice([3, 4, 5])
        if to_glue_ind >= text_len:
            glued_tokens.append(glue_sents(sentences[current_ind:]))
        else:
            glued_tokens.append(glue_sents(sentences[current_ind:to_glue_ind]))
        current_ind = to_glue_ind
    return glued_tokens

# transform our data into glued, tokenized sentences
ted_data = []
for text in tqdm(ted['transcript']):
    ted_data.extend(glue_text(text))
    
# save as json
with open(PATH+'ted_data.json', 'w') as wf:
    json.dump(ted_data, wf)
    

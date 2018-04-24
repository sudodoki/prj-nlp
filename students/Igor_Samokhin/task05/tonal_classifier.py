""" 
Using tonal dictionary to classify texts into positive and negative.
We add sentiments for each word in a text whose lemma has an entry in
a tonal dict. If overall sentiment is non-negative, we mark text as 
positive
"""

import pandas as pd
from tokenize_uk.tokenize_uk import tokenize_words
import pymorphy2
import string
morph = pymorphy2.MorphAnalyzer(lang='uk',
    path='/home/igor/Bin/anaconda3/lib/python3.6/site-packages/pymorphy2_dicts_uk/data')

tonal_df = pd.read_csv('tone-dict-uk.tsv', sep='\t', header=None, names=['word', 'sentiment'])
tonal_df['word'] = tonal_df['word'].str.lower()
tonal_dict = dict(zip(tonal_df['word'], tonal_df['sentiment']))

def tonal_classifier(text, tonal_dict, stopwords):
    tokens = tokenize_words(text)
    previous = []
    total_sent = 0
    for t in tokens:
        w = t.strip().lower()
        w = morph.parse(w)[0].normal_form
        if w.isdigit():
            continue
        elif w in stopwords:
            continue
        elif w in string.punctuation:
            not_flag = False
        elif w in previous:
            continue
        elif w.isalnum():
            previous.append(w)
            if w not in tonal_dict.keys():
                # if word not in dictionary, ignore it
                continue
            else:
                total_sent += tonal_dict[w]
        else:
            continue
    if total_sent >= 0:
        return 'pos'
    else:
        return 'neg'


from collections import Counter
import pandas as pd
import re
import numpy as np

DATA_DIR = '/Users/admin/edu/NLP/practical_NLP_course/data/'
BLOGS_PREP_EN_FILE = 'sample2.tsv'


def age_group(row):
    if row['age'] < 18:
        if row['label'][-1] == 'F':
            return '17F'
        elif row['label'][-1] == 'M':
            return '17M'
    elif row['age'] < 30:
        if row['label'][-1] == 'F':
            return '27F'
        elif row['label'][-1] == 'M':
            return '27M'
    else:
        if row['label'][-1] == 'F':
            return '47F'
        elif row['label'][-1] == 'M':
            return '47M'


header = ['sent_cnt', 'token_cnt', 'word_cnt', 'title_cnt', 'caps_cnt', 'number_cnt', 'smiles_cnt', 'punct_cnt',
          'uniq_cnt', 'words_len', 'big_word_cnt', 'animate_prons', 'first_person_pron', 'reflexive_prons',
          'accent_punct', 'uncertain_word', 'filler_word', 'notional_pos', 'functional_pos', 'syllables',
          'word_freq', 'informal', 'complex_dep', 'pejorative',
          'punct_rate', 'punct_sent_rate', 'token_per_sent', 'word_rate', 'uniq_rate', 'title_rate', 'caps_rate',
          'verb_rate', 'adj_rate', 'pron_rate', 'noun_rate', 'adv_rate', 'det_rate', 'conj_rate', 'unkn_rate',
          'words_len_rate', 'big_words_rate', 'animate_rate', 'first_rate', 'reflexive_rate', 'accent_punct_rate',
          'notional_pos_rate', 'functional_pos_rate', 'syllables_rate', 'word_freq_rate', 'informal_rate',
          'complex_dep_rate', 'pejorative_rate',
          'PUNCT', 'SYM', 'X ', 'ADJ', 'VERB', 'CONJ', 'NUM', 'DET', 'ADV', 'ADP', 'NOUN',
          'PROPN', 'PART', 'PRON', 'INTJ', 'pos_corpus', 'text',
          'age', 'label']

data = pd.read_csv(DATA_DIR + BLOGS_PREP_EN_FILE, encoding='utf-8', sep='\t', header=None, names=header)

data['func_notion_pos'] = data['functional_pos'] / data['notional_pos']
data['func_notion_pos'].fillna(0.0, inplace=True)
data['func_notion_pos'].replace([np.inf, -np.inf], [0.0, 0.0], inplace=True)

data['age'] = data['age'].astype(int)

data['Y'] = data.apply(age_group, axis=1)

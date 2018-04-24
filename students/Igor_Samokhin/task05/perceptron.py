from tokenize_uk.tokenize_uk import tokenize_words
import pandas as pd
import string
import random
import pymorphy2
morph = pymorphy2.MorphAnalyzer(lang='uk',
    path='/home/igor/Bin/anaconda3/lib/python3.6/site-packages/pymorphy2_dicts_uk/data')

def initialize_vocab(vocab):
    """
    A function that takes vocabulary from train set
    and makes all values 1
    """
    initial_vocab = {}
    for w in (vocab['pos'] + vocab['neg']):
        initial_vocab[w] = {'pos': 1, 'neg': 1}
    return initial_vocab
  
def tokenize_text(text, vocab, stopwords):
    tokens = tokenize_words(text)
    res_tokens = []
    for t in tokens:
        w = t.strip().lower()
        w = morph.parse(w)[0].normal_form
        if w.isdigit():
            continue
        elif w in stopwords:
            continue
        elif w in string.punctuation:
            not_flag = False
        elif w.isalnum():
            if w not in vocab.keys():
                # if word not in dictionary, ignore it
                continue
            else:
                res_tokens.append(w)
        else:
            continue
    return res_tokens

def perceptron_classifier(text, vocab, stopwords):
    """
    A function to predict the label
    of the text given perc_vocab
    """
    tokens = tokenize_words(text)
    guess_dict = {'pos': 0, 'neg': 0}
    for w in tokenize_text(text, vocab, stopwords):
        guess_dict['pos'] += vocab[w]['pos']
        guess_dict['neg'] += vocab[w]['neg']
    if guess_dict['pos'] == guess_dict['neg']:
        return random.choice(['pos', 'neg'])
    else:
        return max(guess_dict, key=guess_dict.get)
    
def train_perceptron(n_iter, train_df, vocab, stopwords):
    """
    A function to train perceptron through
    n_iter iterations. Train_df should
    contain 'sent' column with labels
    and 'text' column with texts to train on.
    perc_vocab contains all words with weights.
    """
    for i in range(n_iter):
        for ind, row in train_df.iterrows():
            text = row['text']
            true_lab = row['sent']
            perc_lab = perceptron_classifier(text, vocab, stopwords)
            if perc_lab != true_lab:
                for w in tokenize_text(text, vocab, stopwords):
                    vocab[w][true_lab] += 1
                    vocab[w][perc_lab] -= 1
    return vocab


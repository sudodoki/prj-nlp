import pandas as pd
from collections import defaultdict, Counter
import numpy as np
from tokenize_uk.tokenize_uk import tokenize_words
import string
import random
import pymorphy2
morph = pymorphy2.MorphAnalyzer(lang='uk',
    path='/home/igor/Bin/anaconda3/lib/python3.6/site-packages/pymorphy2_dicts_uk/data')
from build_datasets import stopwords

# build a vocabulary for a classifier
# filter out the stopwords
# count only if a word occurs in a document at least once, not counting next times

def add_vocab_word(text, label, vocab, stopwords):
    """
    A function that adds new words to vocabulary for a class
    """
    tokens = tokenize_words(text)
    previous = []
    not_flag = False
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
            if not_flag:
                w = 'НЕ_' + w
            vocab[label].append(w)
            previous.append(w)
            if w == 'не':
                not_flag = True
        else:
            continue

def build_vocab(train_df, label_col, text_col, stopwords):
    """
    A function that builds a vocabulary for train dataset
    """
    vocab = {}
    for lab in set(train_df[label_col].values):
        vocab[lab] = []
    for i, row in train_df.iterrows():
        lab = row[label_col]
        text = row[text_col]
        add_vocab_word(text, lab, vocab, stopwords)
    return vocab

# calculate prior probabilities
def get_priors(train_df, label_col):
    """
    A function that calculates prior probabilities
    for train dataset
    """
    priors = {}
    n = train_df[label_col].count()
    for lab in set(train_df[label_col].values):
        priors[lab] = np.log(sum(train_df[label_col]==lab)/n)
    return priors

# create probability dictionaries

def train_Naive_Bayes(vocab, labels):
    """
    A function that creates probability distribution
    for each word in each class
    """
    counter_dict = {}
    vocab_all = []
    for lab in labels:
        counter_dict[lab] = Counter(vocab[lab])
        vocab_all += vocab[lab]
    prob_dict = {}
    vocab_set = set(vocab_all)
    for w in vocab_set:
        prob_dict[w] = {}
        for lab in labels:
            # to avoid zero probabilities, we use Laplace (add-one) smoothing
            p_lab = (counter_dict[lab][w] + 1)/(len(vocab[lab]) + len(vocab_set))
            prob_dict[w]['p_'+lab] = np.log(p_lab)
    return prob_dict

def NB_classifier(text, prob_dict, priors, stopwords):
    """
    A function to classify text into positive or negative,
    given dictionary of probabilities from train dataset,
    using a Naive Bayes algorithm
    """
    labels = list(priors.keys())
    tokens = tokenize_words(text)
    # initialize lists of conditional probabilities
    probs = {}
    for lab in labels:
        probs[lab] = []
    previous = []
    not_flag = False
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
            if not_flag:
                w = 'НЕ_' + w
            previous.append(w)
            if w == 'не':
                not_flag = True
            if w not in prob_dict.keys():
                # if word not in dictionary, ignore it
                continue
            else:
                # add Bayes probabilities for both classes
                for lab in labels:
                    probs[lab].append(prob_dict[w]['p_'+lab] + priors[lab])
        else:
            continue
    # calculate sum of log probabilities
    sums_of_log_probs = {}
    for lab in labels:
        sums_of_log_probs[lab] = sum(p for p in probs[lab])
    return max(sums_of_log_probs, key=sums_of_log_probs.get)
import pandas as pd
from collections import defaultdict, Counter
import numpy as np
from nltk.tokenize import word_tokenize
pd.options.mode.chained_assignment = None

# start by importing data
df = pd.read_csv('ukr_reviews.csv.gz', compression='gzip')

# make a column indicating positive or negative sentiment 
df['sent'] = df['rating'].apply(lambda x: 'pos' if x>3 else 'neg')

# let's leave 30% of the data as test set
train = df.sample(int(0.7*len(df)), random_state=505)
test = df[~df.index.isin(train.index)]

# build a vocabulary for a baseline classifier

vocab = {}
vocab['pos'] = []
vocab['neg'] = []

def build_vocab(text, label, vocab):
    tokens = word_tokenize(text)
    for t in tokens:
        if t.isdigit():
            continue
        elif t.isalnum():
            vocab[label].append(t.strip().lower())
        else:
            continue
        
for i, row in train.iterrows():
    label = row['sent']
    text = row['text']
    build_vocab(text, label, vocab)
    
# calculate prior probabilities
pos_prior = (train['sent']=='pos').sum()/len(train)
neg_prior = (train['sent']=='neg').sum()/len(train)
priors = {'pos': pos_prior, 'neg': neg_prior}

# create probability distributions for our bag of words

counter_dict = {'pos': Counter(vocab['pos']), 'neg': Counter(vocab['neg'])}

prob_dict = {}
vocab_set = set(vocab['pos'] + vocab['neg'])
for w in vocab_set:
    # to avoid zero probabilities, use Laplace (add-one) smoothing
    p_pos = (counter_dict['pos'][w] + 1)/(len(vocab['pos']) + len(vocab_set))
    p_neg = (counter_dict['neg'][w] + 1)/(len(vocab['neg']) + len(vocab_set))
    # check that we don't assign zero probabilities
    prob_dict[w] = {'p_pos': p_pos, 'p_neg': p_neg}
    
def baseline_classifier(text, prob_dict, priors):
    """
    A function to classify text into positive or negative,
    given dictionary of probabilities from train dataset,
    using a Naive Bayes algorithm
    """
    tokens = word_tokenize(text)
    pos_prior, neg_prior = priors['pos'], priors['neg']
    # initialize lists of conditional probabilities
    probs = {'pos': [], 'neg': []}
    for t in tokens:
        if t.isdigit():
            continue
        elif t.isalnum():
            w = t.strip().lower()
            if w not in prob_dict.keys():
                # if word not in dictionary, ignore it
                continue
            else:
                # add Bayes probabilities for both classes
                probs['pos'].append(prob_dict[w]['p_pos'] * pos_prior)
                probs['neg'].append(prob_dict[w]['p_neg'] * neg_prior)
        else:
            continue
    # calculate sum of log probabilities
    sum_log_probs_pos = sum([np.log(p) for p in probs['pos']])
    sum_log_probs_neg = sum([np.log(p) for p in probs['neg']])
    if sum_log_probs_pos > sum_log_probs_neg:
        return 'pos'
    else:
        return 'neg'
    
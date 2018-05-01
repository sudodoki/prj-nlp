import string
import re
import itertools
import spacy
import numpy as np
from nltk.tokenize import sent_tokenize
import matplotlib.pyplot as plt


nlp = spacy.load('en', disable=['ner'])

punct = ''.join(set(string.punctuation) - {'!', '.', '?'})
punct_reg = re.compile('[%s]' % re.escape(punct))
num_reg = re.compile('[0-9]')


def plot_confusion_matrix(cm, classes,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    print(cm)

    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    fmt = 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')


def clean(sent):
    res = sent.replace('\n', ' ').replace('\t', ' ')
    res = punct_reg.sub('', res)
    res = num_reg.sub('', res)
    return res.strip()


def combine(sents):
    sents = [clean(x) for x in sents]
    sents = [nlp(x) for x in sents]
    tuples = zip(sents[0:-1], sents[1:])
    tuples = [(x[0], x[1]) for x in tuples if x[0][-1].text == '.']
    result = []
    for f, s in tuples:
        if len(f) < 2:
            continue
        res = []
        for tok in f:
            if tok.text.strip():
                res.append((tok, False))
        res = res[0:-1]
        res[-1] = (res[-1][0], True)
        for tok in s:
            if tok.text.strip():
                res.append((tok, False))
        result.append(res)
    return result


def prepare_data(df): 
    parsed = df.apply(sent_tokenize)
    parsed = parsed.apply(combine)
    parsed = [item for sublist in parsed for item in sublist]
    return parsed


def prepare_test(test):
    overall_result = []
    for test_data in test:
        s = ' '.join([x[0] for x in test_data])
        doc = nlp(s)
        result = []
        for token, (w, l) in zip(doc, test_data):
            result.append({
                'word': token.text,
                'lemma': token.lemma_,
                'pos': token.pos_,
                'tag': token.tag_,
                'dep': token.dep_,
                'label': 1 if l else 0
            })
        overall_result.append(result)
    return overall_result


def add_info(txt):
    result = []
    for w, label in txt:
        result.append({
            'word': w.text,
            'lemma': w.lemma_,
            'pos': w.pos_,
            'tag': w.tag_,
            'dep': w.dep_,
            'label': 1 if label else 0
        })
    return result


def window(txt):
    windows = []
    for i in range(2, len(txt)-1):
        windows.append(txt[i-2:i+2])
    return windows


def extract_features(window):
    prev_2 = window[0]
    prev_1 = window[1]
    current = window[2]
    next_1 = window[3]
    return {
        'word': current['lemma'],
        'tag': current['tag'],
        'pos': current['pos'],
        'dep': current['dep'],
        'word-2': prev_2['lemma'],
        'tag-2': prev_2['tag'],
        'pos-2': prev_2['pos'],
        'dep-2': prev_2['dep'],
        'word-1': prev_1['lemma'],
        'tag-1': prev_1['tag'],
        'pos-1': prev_1['pos'],
        'dep-1': prev_1['dep'],
        'word+1': next_1['lemma'],
        'tag+1': next_1['tag'],
        'pos+1': next_1['pos'],
        'dep+1': next_1['dep'],
        'label': current['label']
    }

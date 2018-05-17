import bz2
from collections import OrderedDict
import numpy as np

ROOT = OrderedDict([('id', 0), ('form', 'ROOT'), ('lemma', 'ROOT'), ('upostag', "ROOT"),
                    ('xpostag', None), ('feats', None), ('head', None), ('deprel', None),
                    ('deps', None), ('misc', None)])

def read_embeddings(filename, word_index=None):
    word_2_vec = {}
    with bz2.open(filename, "rt") as f:
        words, ndim = map(int, f.readline().strip().split())
        for line in f:
            values = line.split()
            word = values[0]
            if words:
                if word in word_index:
                    vec = np.asarray(values[1:], dtype=np.float32)
                    word_2_vec[word] = vec
            else:
                vec = np.asarray(values[1:], dtype=np.float32)
                word_2_vec[word] = vec
    return word_2_vec, ndim, words

def clean_deprel(deprel):
    if ":" in deprel:
        return deprel.split(":")[0]
    return deprel
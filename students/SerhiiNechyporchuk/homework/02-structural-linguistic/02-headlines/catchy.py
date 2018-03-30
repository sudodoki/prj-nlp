from nltk.corpus import sentiwordnet as swn
from nltk.corpus import wordnet as wn
import numpy as np
import nltk
import spacy
import spacy.symbols as ss
import en_core_web_lg
import argparse

def has_proper_name(doc):
    return len([tok for tok in doc if tok.pos == ss.PROPN]) != 0

def spacy_pos2wordnet_pos(pos):
    if pos == ss.NOUN:
        return wn.NOUN
    elif pos == ss.VERB:
        return wn.VERB
    elif pos == ss.ADV:
        return wn.ADV
    elif pos == ss.ADJ:
        return wn.ADJ

def is_content_word(tok):
    return tok.pos == ss.NOUN or tok.tag_.startswith('V') or tok.pos == ss.ADJ or tok.pos == ss.ADV

def get_mean_sentiment(tok, topn):
    wn_pos = spacy_pos2wordnet_pos(tok.pos)
    if not wn_pos:
        return np.nan
    synsets = list(swn.senti_synsets(tok.lemma_, wn_pos))[:topn]
    if len(synsets) == 0:
        return np.nan
    sentiments = map(lambda node: node.pos_score(), synsets)
    mean_sentiment = np.mean(list(sentiments))
    return mean_sentiment

def has_positive_sentiment(doc):
    content_words = filter(is_content_word, doc)
    sentiments = [get_mean_sentiment(tok, topn=5) for tok in content_words]
    if len(sentiments) == 0:
        return False
    else:
        return np.nanmax(sentiments) >= 0.5

def has_superlatives(doc):
    superlatives = list(filter(lambda tok: tok.tag_ in ('JJS', 'RBS'), doc))
    return len(superlatives) != 0

def is_catchy(doc):
    return has_proper_name(doc) or has_positive_sentiment(doc) or has_superlatives(doc)


if __name__ == "__main__":
    argparser = argparse.ArgumentParser(description='filters catchy titles')
    argparser.add_argument('--input', required=True, help='Path to the input file')
    argparser.add_argument('--output', required=True, help='Path to the output file')
    args = argparser.parse_args()

    nltk.download('sentiwordnet', quiet=True)
    nltk.download('wordnet', quiet=True)

    nlp = en_core_web_lg.load()

    with open(args.input) as rf:
        lines = rf.readlines()
        catchy_titles = filter(lambda l: is_catchy(nlp(l)), lines)

    with open(args.output, 'w') as wf:
        wf.writelines(catchy_titles)








from collections import Counter
import argparse

import spacy
from spacy.lemmatizer import Lemmatizer
from spacy.lang.en import LEMMA_INDEX, LEMMA_EXC, LEMMA_RULES

SYNONYM_VERBS = {"say", "tell", "speak", "claim", "communicate", "report", "inform", "declare", "remark",
                 "assert", "express"}

nlp = spacy.load('en_core_web_sm')
lemmatizer = Lemmatizer(LEMMA_INDEX, LEMMA_EXC, LEMMA_RULES)
adv_counter = {k: Counter() for k in SYNONYM_VERBS}


def proceed_sentence(sentence):
    """
    Proceed the sentence from the text
    :param sentence: Sentence to be tokenized
    :return:
    """
    doc = nlp(sentence)
    for token in doc:
        if token.pos_ == 'VERB':
            # get lemma of the verb
            lemmas = set(lemmatizer(token.text, 'VERB'))
            synonyms_in_sentence = lemmas.intersection(SYNONYM_VERBS)
            if synonyms_in_sentence:
                # looks for adverb in the children of the verb
                for child in token.children:
                    if child.pos_ == 'ADV' and child.text[-2:] == 'ly':
                        for word in synonyms_in_sentence:
                            # add adverb to Counter
                            adv_counter[word][child.text.lower()] += 1


def print_result():
    """
    Print the result in proper view
    """
    for word, advs in adv_counter.items():
        if advs:
            print('{}: {}'.format(word, ', '.join(['({}, {})'.format(k, v) for k, v in advs.most_common(10)])))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Input file')
    parser.add_argument('-input', dest='input_path', help='path to input file')
    args = parser.parse_args()
    with open(args.input_path, 'r') as i_f:
        for sentence in i_f:
            proceed_sentence(sentence)
    print_result()

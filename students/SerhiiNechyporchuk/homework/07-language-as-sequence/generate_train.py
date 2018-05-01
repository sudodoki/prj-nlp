import bz2
import en_core_web_lg
import numpy as np
import itertools as it
import json
import sys
import random

def stream_of_sents(f, nlp_sent):
    lines = (bline.decode('utf-8').strip() for bline in f)
    list_of_sents = (nlp_sent(line).sents for line in lines)
    sents = it.chain.from_iterable(list_of_sents)
    valid_sents = filter(lambda xs: (xs[-1].text == '.') and (xs[0].is_title), sents)
    return valid_sents


def get_rand_ngram(iterable):
    piece = list(it.islice(iterable, int(np.random.choice([2,3,4], p=[0.7, 0.2, 0.1]))))
    while piece:
        yield piece
        piece = list(it.islice(iterable, int(np.random.choice([2,3,4], p=[0.7, 0.2, 0.1]))))


def combine_ngrams(ngrams):
    for ngram in ngrams:
        yield [sent.text for sent in ngram]


def generate_train(input, output, nlp_sent):
    with bz2.open(input) as f, bz2.open(output, 'wt') as w:
        try:
            valid_sents = stream_of_sents(f, nlp_sent)
            # print(list(it.islice(valid_sents, 1)))
            ngrams = get_rand_ngram(valid_sents)
            combined = combine_ngrams(ngrams)
            for i, example in enumerate(combined):
                try:
                    if i % 1000 == 0:
                        print(i)
                    w.write(json.dumps(example) + '\n')
                except (KeyboardInterrupt, SystemExit):
                    print("Processed: ", i)
                    break
        except (KeyboardInterrupt, SystemExit):
            print("Exiting")


if __name__ == '__main__':
    print(sys.argv)
    input = sys.argv[1]
    output = sys.argv[2]
    assert input and output
    nlp_sent = en_core_web_lg.load(disable=['parser', 'ner', 'textcat', 'tagger'])
    nlp_sent.add_pipe(nlp_sent.create_pipe('sentencizer'))
    print("Starting")
    generate_train(input, output,  nlp_sent)
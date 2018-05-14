import sys
import re
import en_core_web_lg
from collections import Counter
import bz2
import rocksdb


rules = {
    re.compile(r'(- ){3,}'): '',
    re.compile(r'!+'): '!',
    re.compile(r'\?+'): '?',
    re.compile(r' +'): ' ',
    re.compile(r'\t'): ''
}


def write_counter(f, counter):
    batch = rocksdb.WriteBatch()
    for (k, v) in counter.most_common():
        batch.put(" ".join(k).lower().encode("utf-8"), str(v).encode("utf-8"))
    f.write(batch)


def preprocess(line):
    for r,v in rules.items():
        line = re.sub(r, v, line)
    return line


def collect_ngrams(size, tokens):
    xs = ['<SS>']*(size-1) + tokens + ['</SS>']*(size-1)
    N = len(tokens)
    return Counter([tuple(xs[i:i+size]) for i in range(N + (size-2))])


def gather(file, output_prefix, nlp):
    with bz2.open(file) as f:
        counter_names = [
            '2grams',
            '3grams',
            'pos2grams',
            'pos3grams',
        ]
        counters = {counter_name: Counter() for counter_name in counter_names}
        files = {cn: rocksdb.DB(output_prefix + cn + ".db", rocksdb.Options(create_if_missing=True))
                 for cn in counter_names}

        for i, line in enumerate(f):
            try:
                if (i % 1000 == 0) and (i != 0):
                    try:
                        print(i)
                        for cn, file in files.items():
                            write_counter(file, counters[cn])
                    except (KeyboardInterrupt, SystemExit):
                        pass

                line = line.decode('utf-8').strip()
                line = preprocess(line)
                doc = nlp(line)
                tokens = [tok.text for tok in doc]
                #poss = [tok.pos_ for tok in doc]
                counters['2grams'].update(collect_ngrams(2, tokens))
                counters['3grams'].update(collect_ngrams(3, tokens))
                #counters['pos2grams'].update(collect_ngrams(2, poss))
                #counters['pos3grams'].update(collect_ngrams(3, poss))
            except (KeyboardInterrupt, SystemExit):
                print("Processed {} lines".format(i))
                for cn, file in files.items():
                    write_counter(file, counters[cn])
                break


if __name__ == '__main__':
    print(sys.argv)
    input = sys.argv[1]
    output = sys.argv[2]
    assert input and output
    nlp = en_core_web_lg.load(disable=['parser', 'ner', 'textcat', 'tagger'])
    nlp.add_pipe(nlp.create_pipe('sentencizer'))
    gather(input, output,  nlp)
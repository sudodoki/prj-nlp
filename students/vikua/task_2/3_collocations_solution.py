import os
import argparse
import math
import multiprocessing
from concurrent.futures import ProcessPoolExecutor, as_completed

import spacy


VERBS = {"say", "tell", "speak", "claim", "communicate",
         "express", "mention", "suggest", "state", "assert",
         "announce", "answer", "declare", "disclose", "repeat", "reply",
         "report", "respond", "reveal"}


nlp = spacy.load('en', disable=['ner'])


def is_valid_file(arg):
    """ Auxiliary function to make sure input file path exists.
    """
    if not os.path.exists(arg):
        raise argparse.ArgumentTypeError("The file {} doesn't exist".format(arg))
    return arg


def get_collocations_in_sentence(doc, verbs):
    """ Gets all collocations of verbs and adverbs in provided sentence.
    The logic is:
        - loop over tokens in provided sentence
        - check of token is a verb and it present in requested verbs set
        - check if there are dependent adverbs (advmod dependency), which ends with 'ly'
          and add them to collocation list
        - for each such adverb, check if there are dependent adverbs (conj dependency),
          which ends with 'ly' and add those to collocations list also

    Parameters
    ----------
    doc : Doc
        Parsed sentence
    verbs : set of str
        A set of verbs to check for collocations

    Returns
    -------
    result : dict
        A dictionary with mapping from verb to list of dependent adverbs
    """
    result = dict()
    for token in doc:
        if (token.lemma_ in verbs or token.text.lower() in verbs) and token.pos_ == 'VERB':
            collocations = []
            for c in token.children:
                if c.dep_ == 'advmod' and c.text.endswith('ly'):
                    collocations.append(c.text.lower())
                    for cc in c.children:
                        if cc.dep_ == 'conj' and c.text.endswith('ly'):
                            collocations.append(cc.text.lower())
            result[token.lemma_] = collocations
    return result


def find_all_collocations(content, verbs):
    """ Function, which given a list of sentences and set of verbs, searches for
    verb-adverb collocations.
    Calculates number of times particular verb-adverb collocation was seen in provided text.

    Parameters
    ----------
    content : list of str
        A list of sentences
    verbs : set of str
        A set of verbs to check for collocations

    Returns
    -------
    result : dict
        A dictionary with mapping from verb to another dictionary, where keys are collocated
        adverbs and values are number of times particular verb-adverb collocation was seen in the
        text.
    """
    result = dict()
    for line in content:
        doc = nlp(line)
        doc_col = get_collocations_in_sentence(doc, verbs)
        for verb, adverbs in doc_col.items():
            collocations = result.get(verb, None)
            if not collocations:
                result[verb] = {c: 1 for c in adverbs}
            else:
                for adv in adverbs:
                    count = collocations.get(adv, 0)
                    count = count + 1
                    collocations[adv] = count
                result[verb] = collocations
    return result


def merge_dictionaries(d1, d2):
    """ Merges two dictionaries taking into account that values are dictionaries itself
    """
    def _merge_collocation_number(x, y):
        return {k: x.get(k, 0) + y.get(k, 0) for k in set(x) | set(y)}

    return {k: _merge_collocation_number(d1.get(k, dict()), d2.get(k, dict()))
            for k in set(d1) | set(d2)}


def get_top_frequent_adverbs(collocations, n=10):
    """ Takes only N top adverbs based on number of times particular verb-adverb collocation
     was seen.

    Parameters
    ----------
    collocations: dict
        A dictionary returned from find_all_collocations function
    n : int
        Number of most frequent adverbs to return

    Returns
    -------
    result : dict
        A dictionary with mapping from verb to a list with N most frequent collocated adverbs
        a long with number of times particular adverb was seen in conjunction with the verb \
        (list of tuples)
    """
    for verb, col in collocations.items():
        s = sorted(col.items(), key=lambda k: k[1], reverse=True)
        top_n = s[:n]
        collocations[verb] = top_n
    return collocations


def main(input_file, output_file, verbs):
    with open(input_file) as f:
        content = f.readlines()
        content = [x.replace('\n', '') for x in content]

    cpu_count = multiprocessing.cpu_count()
    with ProcessPoolExecutor(max_workers=cpu_count) as executor:
        chunk_size = int(math.ceil(len(content) / float(cpu_count)))
        index = 0
        futures = []
        for _ in range(cpu_count):
            data = content[index:index + chunk_size]
            index = index + chunk_size

            future = executor.submit(find_all_collocations, data, verbs)
            futures.append(future)

        result = dict()
        for f in as_completed(futures):
            r = f.result()
            print('Process completed')
            result = merge_dictionaries(result, r)

    top = get_top_frequent_adverbs(result)

    with open(output_file, 'w') as f:
        for v, c in top.items():
            f.write('{}: {}\n'.format(v, str(c)))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Task 2.3 Collocations')
    parser.add_argument('--sent', dest='sentence', help='Test sentence to check collocations')
    parser.add_argument('--verb', dest='verb', help='A verb to check collocated adverbs for')
    parser.add_argument('-i', dest='input_file', type=is_valid_file, help='Input file with headlines')
    parser.add_argument('-o', dest='output_file', default='output.csv', help='Output file')

    args = parser.parse_args()

    verbs = VERBS
    if args.verb:
        verbs = {args.verb}

    if args.sentence:
        # just for ad-hoc testing purposes,
        print(get_collocations_in_sentence(nlp(args.sentence), verbs))
    else:
        if os.path.exists(args.output_file):
            os.remove(args.output_file)
        main(args.input_file, args.output_file, verbs)

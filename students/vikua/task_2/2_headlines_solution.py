import argparse
import os

import spacy


def is_valid_file(arg):
    """ Auxiliary function to make sure input file path exists.
    """
    if not os.path.exists(arg):
        raise argparse.ArgumentTypeError("The file {} doesn't exist".format(arg))
    return arg


def get_formatted_headline(doc):
    """ Function which does actual formatting according to Associated Press StyleBook

    1. Capitalize nouns, pronouns, adjectives, verbs, adverbs, and subordinate conjunctions.
       If a word is hyphenated, every part of the word should be capitalized
       (e.g., "Self-Reflection" not "Self-reflection").
    2. Capitalize the first and the last word.
    3. Lowercase all other parts of speech: articles, coordinating conjunctions,
       prepositions, particles, interjections.

    Parameters
    ----------
    doc : Doc
        Representation of one input sentence parsed with spacy

    Returns
    -------
    sentence : str
        Formatted sentence with all required words capitalized
    """
    sentence = []
    i = 0
    while i < len(doc):
        token = doc[i]
        # if token is not last and next token is hyphen and there is no whitespace after token
        # capitalizing both words
        if token.i != len(doc) - 1 and token.nbor().tag_ == 'HYPH' and token.whitespace_ == '':
            second_token = token.nbor(2)
            sentence.append(token.text.capitalize())
            sentence.append('-')
            sentence.append(second_token.text.capitalize())
            sentence.append(second_token.whitespace_)
            # skipping next 3 indexes as those are already added to new sentance\\
            i = i + 3
        else:
            # if token is first or last in input sentence
            # or it is one of ('NOUN', 'PROPN', 'PRON', 'ADJ', 'VERB', 'ADV')
            # or it is subordinate conjunction - capitalizing
            if i == 0 or i == len(doc) - 1 or \
                    token.pos_ in {'NOUN', 'PROPN', 'PRON', 'ADJ', 'VERB', 'ADV'} or \
                    (token.tag_ == 'IN' and token.dep_ == 'mark'):
                sentence.append(token.text.capitalize())
            else:
                # otherwise - lower casing
                sentence.append(token.text.lower())
            sentence.append(token.whitespace_)
            i = i + 1
    return ''.join(sentence)


def format_headlines(input_file, output_file):
    nlp = spacy.load('en')

    with open(input_file) as f:
        content = f.readlines()
        content = [x.replace('\n', '') for x in content]

    doc_gen = (nlp(x) for x in content)

    headlines = ((get_formatted_headline(x), x.text) for x in doc_gen)

    with open(output_file, 'w') as f:
        num_correct = 0
        for formatted, original in headlines:
            if formatted == original:
                num_correct = num_correct + 1
            f.write('{}\n'.format(formatted))

    # for this examiner dataset number of correct headlines is 517
    print('Number of correct headlines {}'.format(num_correct))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Task 2.1 Headlines')
    parser.add_argument('-i', dest='input_file', type=is_valid_file,
                        help='Input file with headlines')
    parser.add_argument('-o', dest='output_file', default='output.csv',
                        help='Output file')

    args = parser.parse_args()

    if os.path.exists(args.output_file):
        os.remove(args.output_file)

    format_headlines(input_file=args.input_file,
                     output_file=args.output_file)
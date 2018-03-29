import os
import argparse
import re

from lxml import etree
from lxml.etree import QName


SYNONYMS_SECTION = "==== synonyma ====([\s\S]*?(?=====))"
SYNONYM_REG = "\[\[(\w+)\]\]"


def is_valid_file(arg):
    """ Auxiliary function to make sure input file path exists.
    """
    if not os.path.exists(arg) or os.path.isdir(arg):
        raise argparse.ArgumentTypeError("Path {} doesn't exist or it is not a file".format(arg))
    return arg


def extract_synonyms(text):
    """ Extracts synonyms from text using regular expressions.
        - identifies the section with all synonyms
        - extracts individual words

    Parameters
    ----------
    text : str
        Plain text extracted from <text></text> xml element of wiktionary dump

    Returns
    -------
    result : list of str
        list of synonym words
    """
    synonyms_match = re.search(SYNONYMS_SECTION, text)
    if synonyms_match:
        synonyms = synonyms_match.group(1)
        words = re.findall(SYNONYM_REG, synonyms)
        return words
    return []


def main(input_file, output_file):
    with open(input_file, 'rb') as input_f, open(output_file, 'w') as output_f:
        current_word = None
        for action, elem in etree.iterparse(input_f, events=('start', 'end')):
            q_name = QName(elem.tag)
            if action == 'start' and q_name.localname == 'title':
                current_word = elem.text
            elif action == 'start' and q_name.localname == 'text':
                if elem.text:
                    synonyms = extract_synonyms(elem.text)
                    if synonyms:
                        output_f.write("{}: {}\n".format(current_word, synonyms))
            elif action == 'end' and q_name.localname == 'page':
                current_word = None


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='CS Wiktionary parser')
    parser.add_argument('-i', dest='input_file', type=is_valid_file,
                        help='Input XML file with wiktionary dump')
    parser.add_argument('-o', dest='output_file',
                        help='Output file where synonyms will be stored')

    args = parser.parse_args()

    if os.path.exists(args.output_file):
        os.remove(args.output_file)

    main(args.input_file, args.output_file)
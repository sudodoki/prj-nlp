"""
1. Capitalize nouns, pronouns, adjectives, verbs, adverbs, and subordinate conjunctions.
If a word is hyphenated, every part of the word should be capitalized (e.g., "Self-Reflection" not "Self-reflection").

2. Capitalize the first and the last word.

3. Lowercase all other parts of speech: articles, coordinating conjunctions, prepositions, particles, interjections.

Write a program that formats a headline according to the rules above. Use any programming language and any NLP toolkit.
"""
import os
import argparse
import spacy

POS_CAPS = {'NOUN', 'PROPN', 'ADJ', 'VERB', 'ADV'}
POS_POTENTIAL_CAPS = {'ADP'}
DEP_CAPS = {'mark'}

nlp = spacy.load('en_core_web_sm')


def prepare_token(token):
    """
    Capitalizes the word if necessary
    :param token: token from the sentence
    :return: String: modified token
    """
    output_token = token.text
    # if the word is CAPSLOCK, pass it
    if output_token.isupper():
        return output_token
    # capitalize by pos
    if token.pos_ in POS_CAPS:
        output_token = output_token.capitalize()
    elif token.pos_ in POS_POTENTIAL_CAPS:
        # capitalize by dep
        if token.dep_ in DEP_CAPS:
            output_token = output_token.capitalize()
        # decapitalize others
        else:
            output_token = output_token.lower()
    else:
        # decapitalize others
        output_token = output_token.lower()
    return output_token


def reformat_header(input_sentance):
    """
    Work with header: tokenize, capitalize first and last word
    :param input_sentance: Header as a String
    :return: Formatted header
    """
    doc = nlp(input_sentance)
    temp_words = []  # array of formatted words
    is_first_word = True  # flag of the 1st word
    last_word = ''  # potential last word
    for token in doc:
        temp_word = token.text
        if token.is_alpha:
            if is_first_word:
                # Capitalize first word
                temp_word = token.text.capitalize()
                is_first_word = False
            else:
                temp_word = prepare_token(token)
            # set the word (token with only letters) as potential last word
            last_word = temp_word
        temp_words.append(temp_word + token.whitespace_)

    # capitalize last word (even if last token is not a word)
    if last_word[0].islower():
        for i, word in enumerate(reversed(temp_words)):
            if word == last_word:
                index = -1 * (i + 1)
                temp_words[index] = word.capitalize()
                break
    return ''.join(temp_words)


def format_headers(input_file, output_file):
    """
    Prepare new file with formatted headers
    :param input_file: path to input file
    :param output_file: path to output file
    """
    total_headers, formatted_counter = 0, 0
    with open(input_file, 'r') as i_f:
        with open(output_file, 'a') as o_f:
            for sentence in i_f:
                sentence = sentence.strip()
                reformatted_headline = reformat_header(sentence)
                if sentence != reformatted_headline:
                    formatted_counter += 1
                total_headers += 1
                o_f.write(reformatted_headline + '\n')
    print('Properly formatted headlines in the input file: {}'.format(total_headers - formatted_counter))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Input and output files')
    parser.add_argument('-input', dest='input_path', help='path to input file')
    parser.add_argument('-output', dest='output_path', help='path to output file')
    args = parser.parse_args()
    # drop file if it is exists
    try:
        os.remove(args.output_path)
    except OSError:
        pass
    format_headers(args.input_path, args.output_path)

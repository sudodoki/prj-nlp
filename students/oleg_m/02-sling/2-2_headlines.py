"""
The paper on defines that a catchy headline has the following features:
1. Prominence
2. Sentiment
3. Superlativeness
4. Proximity
5. Surprise
6. Uniqueness

Write a program that analyzes a headline for prominence (a.k.a, named entities), sentiment, and superlativeness.
For sentiment, check if the average sentiment for the top 5 meanings of word+POS in [SentiWordNet] is above 0.5.

"""
import os
import argparse
import spacy
from nltk.corpus import sentiwordnet as swn
from nltk.corpus.reader.wordnet import WordNetError

examples = ["Game of Thrones: Compendium of characters and their connections",
            "Sweeter than 'The Hummingbird and the Honey Bee'",
            "NU Hosts Illinois in First Nationally Televised Network Game at Welsh-Ryan",
            "The Best Brazilian & bikini waxing how to choose an sf bay area hair removal salon or spa",
            "Back to school, gluten-free style",
            "President Obama's Inauguration: Kelly Clarkson sings 'My Country, 'Tis of Thee'",
            "Use Affirmations for Positive Mothering",
            "Pamper Yourself like a Pharaoh at the Ritz Carlton Denver"]

PROMINENCE = {'PERSON', 'ORG', 'GPE', 'EVENT', 'WORK_OF_ART'}
SUPERLATIVE_TAGS = {'JJR', 'JJS', 'RBR', 'RBS'}
SENTI_WORD_POS = {'ADJ': 'a', 'VERB': 'v', 'NOUN': 'n', 'ADV': 'r'}

nlp = spacy.load('en_core_web_sm')


def is_sentiment(tokens):
    """
    Check if the headline is sentiment
    Access to sentiWord using nltk
    :param tokens: Spacy Doc object: list of tokens
    :return: flag if header is sentiment
    """
    for token in tokens:
        token_pos = token.pos_
        if token_pos in SENTI_WORD_POS:
            # prepare string to nltk format (e.g 'word.a.01')
            word_pos = '{}.{}.0'.format(token.text, SENTI_WORD_POS[token_pos])
            sent_scores = []
            for i in range(5):
                # get the scores of first 5 values if exists
                try:
                    breakdown = swn.senti_synset('{}{}'.format(word_pos, i+1))
                    # nltk response: positive, negative, objective
                    # objective = 1 - (positive + negative)
                    # the word is sentiment if it is not objective
                    sent_scores.append(1.0 - breakdown.obj_score())
                except WordNetError:
                    break
            if sent_scores:
                score = sum(sent_scores)/len(sent_scores)
                if score > 0.5:
                    return True
    return False


def is_prominent(entities):
    """
    Check if the headline is prominent
    Taking into account Spacy entities: Person, Organizations, Location, Events or Art
    If headline contains at least one of proper entity it is prominent
    :param entities: Spacy entities
    :return: flag if header is prominent
    """
    for ent in entities:
        if ent.label_ in PROMINENCE:
            return True
    return False


def is_superlative(tokens):
    """
    Check if the headline is superlative
    Taking into account the spacys' tags
    :param tokens: Spacy Doc object: list of tokens
    :return: flag if header is superlative
    """
    for token in tokens:
        if token.tag_ in SUPERLATIVE_TAGS:
            return True
    return False


def is_catchy_headline(headline):
    """
    Check if the header is catchy.
    Analyze header for prominence, superlativeness and sentiment
    If at least one of attributes is True, the header is catchy
    :param headline: header as a string
    :return: flag if header is catchy
    """
    doc = nlp(headline)
    prominence = is_prominent(doc.ents)
    superlativeness = is_superlative(doc)
    sentiment = is_sentiment(doc)
    return any([prominence, superlativeness, sentiment])


def process_headlines(input_file, output_file):
    """
    Looking for catchy headlines in the file and writes it to the file
    :param input_file: path to input file
    :param output_file: path to output file
    :return:
    """
    catchy_counter = 0
    with open(input_file, 'r') as i_f:
        with open(output_file, 'a') as o_f:
            for sentence in i_f:
                if is_catchy_headline(sentence.strip()):
                    o_f.write(sentence)
                    catchy_counter += 1
    print('Catchy headlines in the input file: {}'.format(catchy_counter))


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
    process_headlines(args.input_path, args.output_path)

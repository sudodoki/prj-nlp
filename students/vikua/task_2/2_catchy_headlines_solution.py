import os
import argparse

import spacy


PROMINENT_ENTITIES = {'PERSON', 'NORP', 'ORG', 'GPE',
                      'LOC', 'PRODUCT', 'EVENT'}
SUPERLATIVE_TAGS = {'JJR', 'JJS', 'RBR', 'RBS'}


senti_word_net_pos_tags = {
    'ADJ': 'a',
    'ADV': 'r',
    'NOUN': 'n',
    'VERB': 'v',
}


def is_valid_file(arg):
    """ Auxiliary function to make sure input file path exists.
    """
    if not os.path.exists(arg):
        raise argparse.ArgumentTypeError("The file {} doesn't exist".format(arg))
    return arg


def get_prominence(doc):
    """ Very simple heuristic for computation of prominence

    It checks whether headline has named entities like people, nationalities,
    organizations, countries, other locations, products or events
    and returns number of such entities.

    Original paper http://www.aclweb.org/anthology/E17-4007 makes more sophisticated
    assumptions regarding prominence of the headline. It checks whether entities are
    elite nations and people or celebrities. Also it performs Wikification to get a
    Wikipedia page of the entity and check number of daily page views to approximate
    prominence as amount of online attention an entity get.

    Parameters
    ----------
    doc : Doc
        Headline parsed with spacy

    Returns
    -------
    Number of "prominent" entities.
    """
    if not doc.ents:
        return 0

    doc_entities = {d.label_ for d in doc.ents}
    prom_doc_entities = doc_entities.intersection(PROMINENT_ENTITIES)

    return len(prom_doc_entities)


def get_superlativeness(doc):
    """ Computes superlativeness score for headline

    If headline has comparative or superlative adjective or adverb - it is considered as
    superlative.
    Thus superlativeness is computed as number words with 'JJR', 'JJS', 'RBR', 'RBS' POS tags
    divided by number of words in a headline (punctuation and symbols filtered out).

    "Automatic Extraction of News Values from Headline Text" paper uses only content words
    as denominator for superlativeness computation (wikified entities are also filtered out),
    but heuristic here is a bit simplified.
    """
    words_only = [d for d in doc if d.pos_ not in {'PUNCT', 'SYM'}]
    c = len(words_only)
    penn_tags = {token.tag_ for token in doc}
    superlative_count = len(penn_tags.intersection(SUPERLATIVE_TAGS))
    return  superlative_count / float(c)


def _get_sentiment_dict(senti_net_lines):
    """ Function which takes data from SentiWordNet and creates
    a dictionary with mapping between word and sentiment score.

    Sentiment score is calculated taking into account a difference
    between PosS and NegS (positive and negative score) as well as
    the rank of particular word specified after sharp (like easily#3).

    Logic is taken from SentiWordNet java example.

    Parameters
    ----------
    senti_net_lines : list of str
        list of lines from SentiWordNet dataset

    Returns
    -------
    scores : dict
        A dictionary with mapping between word and sentiment score
    """
    scores = dict()
    temp_dict = dict()

    for line in senti_net_lines:
        if line[0] == '#':
            continue

        data = line.strip().split('\t')
        word_type_marker = data[0]

        if len(data) != 6:
            continue

        # synset score = PosS - NegS
        synset_score = float(data[2]) - float(data[3])

        synset_terms = data[4].split(' ')
        for term in synset_terms:
            term_and_rank = term.split('#')
            syn_term = '{}#{}'.format(term_and_rank[0], word_type_marker)

            syn_term_rank = int(term_and_rank[1])

            term_list = temp_dict.get(syn_term, [])
            term_list.append((syn_term_rank, synset_score))
            temp_dict[syn_term] = term_list

    for (syn_term, rank_list) in temp_dict.items():
        score = 0
        ssum = 0
        for rank, score in rank_list:
            score = score + score / float(rank)
            ssum = ssum + 1.0 / float(rank)
        score = score / ssum
        scores[syn_term] = score

    return scores


def get_sentiment(doc, sentiment_scores, n=5):
    """ Function which computes sentiment.
    Sentiment scores looked up in dictionary created from SentiWordNet dataset.

    The logic is following: if word is a noun, verb, adverb or adjective

    """
    scores = []
    for token in doc:
        text = token.text.lower()
        lemma = token.lemma_
        pos = senti_word_net_pos_tags.get(token.pos_, None)
        if pos:
            score = sentiment_scores.get('{}#{}'.format(text, pos), None)
            if score is None:
                score = sentiment_scores.get('{}#{}'.format(lemma, pos), 0)
        else:
            score = 0
        scores.append(score)

    tokens_with_scores = sorted(zip(doc, scores), key=lambda k: k[1], reverse=True)

    top_n = tokens_with_scores[:n]
    positive_score = sum([x[1] for x in top_n]) / float(n)

    bottom_n = tokens_with_scores[-n - 1:]
    negative_score = sum([x[1] for x in bottom_n]) / float(n)

    if positive_score >= 0.5:
        return 'positive:{}'.format(positive_score)
    elif negative_score <= -0.5:
        return 'negative:{}'.format(negative_score)
    else:
        return 'objective:{}:{}'.format(positive_score, negative_score)


def is_catchy(prominence, sentiment, superlativeness):
    """ Headline is catchy if any of following is true:
        - it has at least one prominent entity;
        - sentiment either positive or negative;
        - superlativeness
    """
    return prominence > 0 or 'objective' not in sentiment or superlativeness > 0


def main(input_file, senti_net_file, output_file):
    nlp = spacy.load('en')

    with open(input_file) as f:
        content = f.readlines()
        content = [x.replace('\n', '') for x in content]

    doc_gen = (nlp(x) for x in content)

    with open(senti_net_file) as f:
        senti_net_lines = f.readlines()

    sentiment_scores = _get_sentiment_dict(senti_net_lines)

    with open(output_file, 'w') as f:
        f.write('text|prominence|sentiment|superlativeness\n')

        for doc in doc_gen:
            p = get_prominence(doc)
            sent = get_sentiment(doc, sentiment_scores)
            sup = get_superlativeness(doc)

            if is_catchy(p, sent, sup):
                # save headline and computed values only if catchy
                f.write('{}|{}|{}|{}\n'.format(doc.text, p, sent, sup))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Task 2.2 Catchy Headlines')
    parser.add_argument('-i', dest='input_file', type=is_valid_file,
                        help='Input file with headlines')
    parser.add_argument('-o', dest='output_file', default='output.csv',
                        help='Output file')
    parser.add_argument('-s', dest='senti_net_file', type=is_valid_file,
                        default='SentiWordNet_3.0.0.txt',
                        help='SentiWorkNet file path')

    args = parser.parse_args()

    if os.path.exists(args.output_file):
        os.remove(args.output_file)

    main(args.input_file, args.senti_net_file, args.output_file)
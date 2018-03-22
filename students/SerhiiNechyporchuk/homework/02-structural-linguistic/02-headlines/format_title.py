import spacy
import spacy.symbols as ss
import en_core_web_lg
import pandas as pd
import csv
import sys
import argparse

def reformat_title(title):
    arr = list(title)
    doc = nlp(title)
    for token in doc:
        if token.text == "n't":
            continue
        if token.pos in (ss.NOUN, ss.PROPN, ss.VERB, ss.ADJ, ss.ADV):
            arr[token.idx] = arr[token.idx].upper()
        elif token.pos == ss.ADP and token.dep != ss.prep: # filter out cconj
            arr[token.idx] = arr[token.idx].upper()
        else:
            arr[token.idx] = arr[token.idx].lower()
    arr[0] = arr[0].upper()
    arr[doc[-1].idx] = arr[doc[-1].idx].upper()
    return ''.join(arr)

if __name__ == '__main__':
    argparser = argparse.ArgumentParser(description='reformats titles')
    argparser.add_argument('--input', required=True, help='Path to the input file')
    argparser.add_argument('--output', required=True, help='Path to the output file')
    args = argparser.parse_args()

    nlp = en_core_web_lg.load()

    df = pd.read_table(args.input, names=['title'])
    df = df.assign(formatted = df['title'].apply(reformat_title))

    num_correct = (df['title'] == df['formatted']).sum()
    total = len(df)

    df[['formatted']].to_csv(
        args.output, header=False, index=False, quoting=csv.QUOTE_NONE, sep='\t')

    print('{} out of {} were correctly formatted'.format(num_correct, total))

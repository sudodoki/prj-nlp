import argparse
import os

import numpy as np
import pandas as pd
import spacy
from nltk.tokenize import sent_tokenize
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MultiLabelBinarizer
from sklearn.metrics import f1_score

from rules import *


class RuleModel(object):

    def __init__(self, rules=None):
        self.rules = rules
        if not self.rules:
            self.rules = [ActorPerformanceMentionRule(),
                          ActorCastRule(),
                          FilmStartsRule()]
        self.nlp = spacy.load('en')
            
    def analyse(self, X):
        """ Parses text, extracts tokens, ner, etc. using spacy

        Parameters
        ----------
        X : pd.Series
            raw paragraphs of text

        Returns
        -------
        result : pd.Series
            a series where each element is a list of spacy docs
        """        
        def parse(text):
            sentences = sent_tokenize(text)
            return [self.nlp(sent) for sent in sentences]

        return X.apply(parse)
    
    def apply_all_rules(self, sentences):
        """ Applies all rules to single sentence.
        Resulting list of actors is union of all rules results.

        Parameters
        ----------
        sentences : list of doc
            List of analyzed sentences

        Returns
        -------
        result : set of str
            set of detected actor names
        """
        result = set()
        for rule in self.rules:
            result = result | set(rule.apply(sentences))
        result = [x for x in result if len(x.split()) <= 3]
        if not result: 
            return ['no']
        else: 
            return result

    def predict(self, X, analyse=True):
        if analyse:  
            X = self.analyse(X)
        pred = X.apply(self.apply_all_rules)
        return pred


def build_dataframe(path, files):
    dfs = []
    for file in files:
        df = pd.read_csv(os.path.join(path, file))
        df['movie'] = file
        dfs.append(df)
    return pd.concat(dfs)


def score_row(y, y_hat):
    mlb = MultiLabelBinarizer()
    matrix = mlb.fit_transform([y, y_hat])
    return f1_score(matrix[0].reshape(-1, 1), matrix[1].reshape(-1, 1))    


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Rule based actor extraction model')
    parser.add_argument('-i', dest='input_dir', help='Path to directory with input data')
    parser.add_argument('-o', dest='output_dir', help='Path to ouput dir')

    args = parser.parse_args()

    files = os.listdir(args.input_dir)

    train_files, test_files = train_test_split(files, train_size=0.5, random_state=1234)

    train_df = build_dataframe(args.input_dir, train_files)
    test_df = build_dataframe(args.input_dir, test_files)

    model = RuleModel()
    train_df['pred'] = model.predict(train_df['paragraph'])
    train_df['cast'] = train_df['cast'].apply(lambda x: x.split(','))

    scores = train_df[['cast', 'pred']].apply(lambda x: score_row(x['cast'], x['pred']), axis=1)
    print('Train F1 score = {}'.format(scores.mean()))

    test_df['pred'] = model.predict(test_df['paragraph'])
    test_df['cast'] = test_df['cast'].apply(lambda x: x.split(','))

    scores = test_df[['cast', 'pred']].apply(lambda x: score_row(x['cast'], x['pred']), axis=1)
    print('Test F1 score = {}'.format(scores.mean()))

    print('Saving results')

    train_df.to_csv(os.path.join(args.output_dir, 'train_result.csv'))
    test_df.to_csv(os.path.join(args.output_dir, 'test_result.csv'))

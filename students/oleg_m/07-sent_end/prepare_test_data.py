import os
import re
import json
import pandas as pd
import spacy, en_core_web_sm


def process_one_token(token):
    return dict(is_aplpha=token.is_alpha, is_digit=token.is_digit, is_lower=token.is_lower, is_punct=token.is_punct,
                is_title=token.is_title, pos=token.pos_, lemma=token.lemma_, token_text=token.text)


def add_n_to_feature_name(prefix, features):
    return {'{}_{}'.format(prefix, k): v for k, v in features.items()}


def prepare_this_features(test_tokens):
    token_features = []
    i = 0
    for token in test_tokens:
        doc = nlp(token)
        token_features.append(process_one_token(doc[0]))
        i += 1
        if i % 100 == 0:
            print(i)
    return token_features


def prepare_three_grams(this_token, this_features):
    this_lemma = this_features['n_lemma']
    threegram = '{} {} {}'.format(this_features.get('nb1_n_token_text'), this_token,
                                  this_features.get('na1_token_text'))
    threegram_lemma = '{} {} {}'.format(this_features.get('nb1_n_lemma'), this_lemma,
                                        this_features.get('na1_lemma'))
    return threegram, threegram_lemma


def prepare_features(features, flags):
    len_current = 0
    # tokens_prev = []
    tokens_data = []
    for i, feature in enumerate(features):
        # tokens_prev.append(token)
        token_data = add_n_to_feature_name('n', feature)
        if len_current > 0:
            token_data = dict(token_data, **add_n_to_feature_name('nb1', {k: v for k, v in tokens_data[-1].items()
                                                                          if k.startswith('n_')}))
        if len_current > 1:
            token_data = dict(token_data, **add_n_to_feature_name('nb2', {k: v for k, v in tokens_data[-2].items()
                                                                          if k.startswith('n_')}))
        for j in range(1, 3):
            try:
                token_data = dict(token_data, **add_n_to_feature_name('na{}'.format(j), features[j]))
            except IndexError:
                break

        token_data['threegrams'], token_data['threegrams_lemma'] = prepare_three_grams(token_data.get('n_token_text'),
                                                                                       token_data)
        token_data['label'] = flags[i]
        tokens_data.append(token_data)
        len_current += 1
    return tokens_data


dir_project = 'prj-nlp'
testfile_path = 'tasks/07-language-as-sequence/run-on-test.json'

dir_path = os.getcwd()
testfile_abspath = re.sub(r'students\/(.+)', 'tasks/07-language-as-sequence/run-on-test.json', dir_path)
testfile = json.load(open(testfile_abspath))
print(testfile_abspath)
nlp = en_core_web_sm.load()


tokens_flags = [item for sublist in testfile for item in sublist]
tokens = (x[0] for x in tokens_flags)
flags = [x[1] for x in tokens_flags]
token_n_features = prepare_this_features(tokens)
features = prepare_features(token_n_features, flags)
pd.DataFrame(features).to_csv('test.csv', index=False)

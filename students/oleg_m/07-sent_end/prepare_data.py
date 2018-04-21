import re
import json
import spacy, en_core_web_sm
import pandas as pd

data_path = '/Users/admin/edu/NLP/practical_NLP_course/data/'
email_threads = 'lucene-threads_sample.json'
other_threads = 'train-v1.1.json'
corpus_xml = 'corpus.xml'
csv_gb = 'emails.csv'

LINK_MAIL_REG = r'(?:https?://|www\.)[^\s]+|mailto:\s?[^\s]+|\b[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}|etc\.\b'
CLEAN_REG = r'^\s*(?:[->:<]|name=|.*wrote.*:)'
CUT_REG = r'^\s*(?:Cheers|(?:Best )?Regards|_{3,}|To unsubscribe|For additional commands' \
          r'|Thanks?(?: you)?,$|From:|To:|Subject:|Sent:|Date:)'


def handle_text(text):
    temp_text = re.sub(LINK_MAIL_REG, ' ', text)
    temp_text = re.sub(r'\.{2,}', ' ', temp_text)
    temp_text = re.sub(r'\(.*\)', ' ', temp_text)
    temp_text = re.sub(r'!+', '.', temp_text)
    temp_text = re.sub(r'\b\d+\b', '9999', temp_text)
    return re.sub(r'\s+', ' ', temp_text)


def cut_attach(str_list):
    for i, line in enumerate(str_list):
        if re.match(CUT_REG, line, re.IGNORECASE):
            return str_list[:i]
    return str_list


def prepare_lucene_threads(input_file):
    data = json.load(open(data_path + input_file))
    texts = (item.get('Body', [])[0] for sublist in data for item in sublist.get('emails', []))
    messages = []
    for i, message in enumerate(texts):
        if type(message) == list:
            message = message[0]
            if type(message) == list:
                message = ''
        text = cut_attach([handle_text(x) for x in message.split('\n') if not re.match(CLEAN_REG, x)])
        messages.append(re.sub(r'\s+', ' ', ' '.join(text)).strip())
    return messages


def process_one_token(token):
    return dict(is_aplpha=token.is_alpha, is_digit=token.is_digit, is_lower=token.is_lower, is_punct=token.is_punct,
                is_title=token.is_title, pos=token.pos_, lemma=token.lemma_, token_text=token.text)


def add_n_to_feature_name(prefix, features):
    return {'{}_{}'.format(prefix, k): v for k, v in features.items()}


def prepare_three_grams(this_token, end_flag, this_features):
    this_lemma = this_features['n_lemma']
    threegram = '{} {} {}'.format(this_features.get('nb1_n_token_text'), this_token,
                                  this_features.get('na1_token_text'))
    threegram_lemma = '{} {} {}'.format(this_features.get('nb1_n_lemma'), this_lemma,
                                        this_features.get('na1_lemma'))
    if end_flag:
        with open('end_of_sents.tsv', 'a') as end_files:
            end_files.write('{}\t{}\t{}\t{}\n'.format(threegram, threegram_lemma, this_token, this_lemma))
            # end_files.write(threegram + '\t' + threegram_lemma + '\n')
    return threegram, threegram_lemma


def process_texts(texts_list, csv_file):
    # df = pd.DataFrame()
    for i, text in enumerate(texts_list):
        if i == 0:
            sample = pd.DataFrame(prepare_texts(text))
            sample.to_csv(csv_file, index=False)
        # elif i == 10:
        #     return
        else:
            pd.DataFrame(prepare_texts(text)).to_csv(csv_file, mode='a', header=False, index=False)
            # df = df.append(pd.DataFrame(prepare_texts(text)))
            print(i)
    # return df.reset_index(drop=True)
    return


def prepare_texts(text):
    tokens = []
    features = []
    doc = nlp(text)
    # is text has 2 or more sents
    sents_gen = doc.sents
    for i in range(2):
        try:
            next(sents_gen)
        except StopIteration:
            return
    for token in doc:
        if not re.match(r'^\s*$|^[\"\']$', token.text) and len(token.text) < 30:
            tokens.append(token.text)
            features.append(process_one_token(token))
    len_tokens = len(tokens)
    len_current = 0

    # tokens_prev = []
    tokens_data = []
    for i, token in enumerate(tokens):
        end_flag = False
        if token == '.' and len_tokens > i + 1:
            continue
        # tokens_prev.append(token)
        token_data = add_n_to_feature_name('n', features[i])
        if len_current > 0:
            token_data = dict(token_data, **add_n_to_feature_name('nb1', {k: v for k, v in tokens_data[-1].items()
                                                                          if k.startswith('n_')}))
        if len_current > 1:
            token_data = dict(token_data, **add_n_to_feature_name('nb2', {k: v for k, v in tokens_data[-2].items()
                                                                          if k.startswith('n_')}))
        j = i + 1
        n = 1
        while True:
            try:
                if tokens[j] == '.':
                    if n == 1:
                        end_flag = True
                    j += 1
                else:
                    token_data = dict(token_data, **add_n_to_feature_name('na{}'.format(n), features[j]))
                    n += 1
                    j += 1
                if n > 2:
                    break
            except IndexError:
                break

        # if len_tokens > i + 3:
        #     if tokens[i + 1] == '.':
        #         end_flag = True
        #         token_data = dict(token_data, **add_n_to_feature_name('na1', features[i + 2]))
        #         token_data = dict(token_data, **add_n_to_feature_name('na2', features[i + 3]))
        #     else:
        #         token_data = dict(token_data, **add_n_to_feature_name('na1', features[i + 1]))
        #         token_data = dict(token_data, **add_n_to_feature_name('na2', features[i + 2]))
        # elif len_tokens == i + 3:
        #     if tokens[i + 1] == '.':
        #         end_flag = True
        #         token_data = dict(token_data, **add_n_to_feature_name('na1', features[i + 2]))
        #     else:
        #         token_data = dict(token_data, **add_n_to_feature_name('na1', features[i + 1]))
        #         token_data = dict(token_data, **add_n_to_feature_name('na2', features[i + 2]))
        # elif len_tokens == i + 2:
        #     token_data = dict(token_data, **add_n_to_feature_name('na1', features[i + 1]))
        token_data['threegrams'], token_data['threegrams_lemma'] = prepare_three_grams(token, end_flag, token_data)
        token_data['label'] = end_flag
        tokens_data.append(token_data)
        len_current += 1
        # labels.append(end_flag)
    # print(tokens_prev)
    return tokens_data
    # return tokens_prev, token_data, labels


nlp = en_core_web_sm.load()

lucene_corpus = prepare_lucene_threads(email_threads)
process_texts(lucene_corpus, 'lucene_corpus.csv')


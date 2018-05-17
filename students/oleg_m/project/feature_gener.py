from collections import Counter
import pandas as pd
import re
import spacy, en_core_web_sm

DATA_DIR = '/Users/admin/edu/NLP/practical_NLP_course/data/'
BLOGS_PREP_EN_FILE = 'blog_authors_preped.csv.gzip'
SMILES = {":‑)", ":)", ":-]", ":]", ":-3", ":3", ":->", ":>", "8-)", "8)", ":-}", ":}", ":o)", ":c)", ":^)", "=]",
          "=)", ":‑D", ":D", "8‑D", "8D", "x‑D", "xD", "X‑D", "XD", "=D", "=3", "B^D", ":-))", ":‑(", ":(", ":‑c",
          ":c", ":‑<", ":<", ":‑[", ":[", ":-||", ">:[", ":{", ":@", ">:(", ":'‑(", ":'(", ":'‑)", ":')", ":-*", ":*",
          ":×", ";‑)", ";)", "*-)", "*)", ";‑]", ";]", ";^)", ":‑,", ";D", ":‑/", ":/", ":‑.", ">:\\", ">:/", ":\\",
          "=/", "=\\", ":L", "=L", ":S", ":‑|", ":|", ":$", ":‑X", ":X", ":‑#", ":#", ":‑&", ":&", "%‑)", "%)"}


class Features():
    def __init__(self):
        self.unique_lemmas = set()
        self.sent_cnt = 1
        self.token_cnt = 0
        self.word_cnt = 0
        self.title_cnt = 0
        self.caps_cnt = 0
        self.number_cnt = 0
        self.smiles_cnt = 0
        self.punct_cnt = 0
        self.pos_dict = Counter()
        self.uniq_cnt = 0
        # RATES
        self.punct_rate = 0.0
        self.punct_sent_rate = 0.0
        self.token_per_sent = 0.0
        self.word_rate = 0.0
        self.uniq_rate = 0.0
        self.title_rate = 0.0
        self.caps_rate = 0.0
        self.verb_rate = 0.0
        self.adj_rate = 0.0
        self.pron_rate = 0.0
        self.noun_rate = 0.0
        self.adv_rate = 0.0
        self.det_rate = 0.0
        self.conj_rate = 0.0
        self.unkn_rate = 0.0
        # POS
        self.PUNCT = 0
        self.SYM = 0
        self.X = 0
        self.ADJ = 0
        self.VERB = 0
        self.CONJ = 0
        self.NUM = 0
        self.DET = 0
        self.ADV = 0
        self.ADP = 0
        self.NOUN = 0
        self.PROPN = 0
        self.PART = 0
        self.PRON = 0
        self.INTJ = 0

    def set_pos_values(self):
        for k, v in self.pos_dict.items():
            setattr(self, k, v)

    def calculate_rates(self):
        if self.token_cnt > 0:
            self.punct_rate = self.punct_cnt / self.token_cnt
            self.word_rate = self.word_cnt / self.token_cnt
            self.verb_rate = self.pos_dict.get('VERB', 0.0) / self.token_cnt
            self.adj_rate = self.pos_dict.get('ADJ', 0.0) / self.token_cnt
            self.pron_rate = self.pos_dict.get('PRON', 0.0) / self.token_cnt
            self.noun_rate = (self.NOUN + self.PROPN) / self.token_cnt
            self.adv_rate = self.ADV / self.token_cnt
            self.det_rate = self.DET / self.token_cnt
            self.conj_rate = (self.ADP + self.CONJ) / self.token_cnt
            self.unkn_rate = self.X / self.token_cnt
        if self.sent_cnt > 0:
            self.punct_sent_rate = self.punct_cnt / self.sent_cnt
            self.token_per_sent = self.token_cnt / self.sent_cnt
        if self.word_cnt > 0:
            self.uniq_rate = self.uniq_cnt / self.word_cnt
            self.title_rate = self.title_cnt / self.word_cnt
            self.caps_rate = self.caps_cnt / self.word_cnt

    def to_array(self):
        return [self.sent_cnt,
                self.token_cnt,
                self.word_cnt,
                self.title_cnt,
                self.caps_cnt,
                self.number_cnt,
                self.smiles_cnt,
                self.punct_cnt,
                self.uniq_cnt,
                # RATES
                self.punct_rate,
                self.punct_sent_rate,
                self.token_per_sent,
                self.word_rate,
                self.uniq_rate,
                self.title_rate,
                self.caps_rate,
                self.verb_rate,
                self.adj_rate,
                self.pron_rate,
                self.noun_rate,
                self.adv_rate,
                self.det_rate,
                self.conj_rate,
                self.unkn_rate,
                # POS
                self.PUNCT,
                self.SYM,
                self.X,
                self.ADJ,
                self.VERB,
                self.CONJ,
                self.NUM,
                self.DET,
                self.ADV,
                self.ADP,
                self.NOUN,
                self.PROPN,
                self.PART,
                self.PRON,
                self.INTJ]


classes_map = {
    '(12.0, 17.0]male': '17M', '(17.0, 25.0]male': '25M', '(25.0, 48.0]male': '48M',
    '(12.0, 17.0]female': '17F', '(17.0, 25.0]female': '25F', '(25.0, 48.0]female': '48F'
}


def prepare(text):
    text = re.sub(r'[\s_]+', ' ', text).strip(' _')
    doc = nlp(text)
    unique_lemmas = set()
    features = Features()
    for token in doc:
        features.token_cnt += 1
        features.sent_cnt += token.is_sent_start if token.is_sent_start else 0
        features.word_cnt += token.is_alpha
        features.title_cnt += token.is_title
        features.caps_cnt += token.is_upper
        features.number_cnt += token.is_digit
        if token.is_punct:
            if token.string in SMILES:
                features.smiles_cnt += 1
            else:
                features.punct_cnt += 1
        if token.is_alpha:
            unique_lemmas.add(token.lemma_)
        features.pos_dict[token.pos_] += 1
    features.uniq_cnt = len(unique_lemmas)
    features.set_pos_values()
    features.calculate_rates()
    return features.to_array()


""" # PREPARATION
data = pd.read_csv(DATA_DIR+BLOGS_PREP_EN_FILE, compression='gzip')
test_sample_100 = data.sample(n=100)
test_sample_1000 = data.sample(n=1000)
test_sample_10p = data.sample(frac=0.1)

test_sample_100.to_csv(DATA_DIR+'test_sample_100.csv')
test_sample_1000.to_csv(DATA_DIR+'test_sample_1000.csv')
test_sample_10p.to_csv(DATA_DIR+'test_sample_10p.csv')
"""

nlp = en_core_web_sm.load()
data = pd.read_csv(DATA_DIR+BLOGS_PREP_EN_FILE, encoding='utf-8', compression='gzip', skiprows=400000, nrows=99999, header=None)
# test_sample_100 = pd.read_csv(DATA_DIR + 'test_sample_1000.csv', encoding='utf-8')[['text', 'age+sex']]

with open('test_file5.tsv', 'a') as tf:
    for i, row in data.iterrows():
        z = prepare(row[0])
        z.append(classes_map[row[3]])
        tf.write('\t'.join(str(x) for x in z) + '\n')
        if i % 1000 == 0:
            print(i)


# test_sample_100['new'] = test_sample_100['text'].apply(prepare_features)
# df = data.text.apply(prepare_features).apply(pd.Series).fillna(0).astype(int)
# df['y'] = data['age+sex'].apply(lambda x: classes_map[x])

# NEW FEATURE RATES
# df['punct_rate'] = df['punct_cnt'] / df['token_cnt']
# df['punct_sent_rate'] = df['punct_cnt'] / df['sent_cnt']
# df['token_per_sent'] = df['token_cnt'] / df['sent_cnt']
# df['word_rate'] = df['word_cnt'] / df['token_cnt']
# df['uniq_rate'] = df['uniq_cnt'] / df['word_cnt']
# df['title_rate'] = df['title_cnt'] / df['word_cnt']
# df['caps_rate'] = df['caps_cnt'] / df['word_cnt']
# df['verb_rate'] = df['pos_VERB'] / df['token_cnt']
# df['adj_rate'] = df['pos_ADJ'] / df['token_cnt']
# df['pron_rate'] = df['pos_PRON'] / df['token_cnt']

# df.to_csv(DATA_DIR + 'blog_authors_features.csv.gzip', compression="gzip")

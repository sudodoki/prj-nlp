from collections import Counter
import re
from wordfreq import word_frequency

DATA_DIR = '/Users/admin/edu/NLP/practical_NLP_course/data/'
BLOGS_PREP_EN_FILE = 'blog_authors_preped.csv.gzip'

SMILES = {":‑)", ":)", ":-]", ":]", ":-3", ":3", ":->", ":>", "8-)", "8)", ":-}", ":}", ":o)", ":c)", ":^)", "=]",
          "=)", ":‑D", ":D", "8‑D", "8D", "x‑D", "xD", "X‑D", "XD", "=D", "=3", "B^D", ":-))", ":‑(", ":(", ":‑c",
          ":c", ":‑<", ":<", ":‑[", ":[", ":-||", ">:[", ":{", ":@", ">:(", ":'‑(", ":'(", ":'‑)", ":')", ":-*", ":*",
          ":×", ";‑)", ";)", "*-)", "*)", ";‑]", ";]", ";^)", ":‑,", ";D", ":‑/", ":/", ":‑.", ">:\\", ">:/", ":\\",
          "=/", "=\\", ":L", "=L", ":S", ":s", ":‑|", ":|", ":$", ":‑X", ":X", ":‑#", ":#", ":‑&", ":&", "%‑)", "%)",
          "<3"}

ANIMATE_PRON = {"i", "we", "he", "she"}
FIRST_PERS = {'i', 'me', 'mine', 'my', 'myself'}
UNCERTAN_WORDS = {'maybe', 'probably', 'possibly', 'perhaps', 'conceivably', 'feasibly', 'imaginably', 'potentially',
                  'apparently', 'likely', 'believably'}
FILLER_WORDS = {'basically', 'actually', 'that', 'really', 'slightly', 'almost', 'seemed',
                'perhaps', 'maybe',  'somehow', 'absolutely', 'seriously', 'so', 'exactly', 'certainly'}
INFORMAL_CONTR = {"ain't", 'gimme', 'gotta', 'gonna', 'kinda', 'lemme', 'wanna', 'whatcha', 'ya', 'oughta', 'lotsa',
                  'outta', 'dunno', 'shoulda', 'coulda', 'woulda', 'cuz', 'coz', 'cos', 'haveta', 'sorta',
                  'betcha', 'tseasy', 'willya', 'dontcha', 'didntcha', 'wontcha', 'whatcha', 'watcha', 'gotcha',
                  'betcha', 'mighta', 'musta', 'couldna', 'shouldna', 'wouldna', 'she\'da', 'he\'da', 'I\'da',
                  'they\'da', 'you\'da', 'lotta', 'cuppa', 'hafta', 'hasta', 'needa', 'supposeta', 'useta',
                  "c'mon", "s'more"}
NOTION_POS = {'ADJ', 'VERB', 'NOUN', 'ADV', 'PROPN'}
FUNCTIONAL_POS = {'ADP', 'CONJ', 'AUX', 'CCONJ', 'DET', 'INTJ', 'SCONJ'}
h = {'just', 'only', 'simply', 'very'}
COMPLEX_DEPS = {'xcomp', 'acl', 'ccomp', 'advcl'}
PEJORATIVE = set(line.strip().lower() for line in open('Pejorative.txt'))

classes_map = {
    '(12.0, 17.0]male': '17M', '(17.0, 25.0]male': '25M', '(25.0, 48.0]male': '48M',
    '(12.0, 17.0]female': '17F', '(17.0, 25.0]female': '25F', '(25.0, 48.0]female': '48F'
}


class Features(object):
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
        self.words_len = 0
        self.big_word_cnt = 0
        self.animate_prons = 0
        self.first_person_pron = 0
        self.reflexive_prons = 0
        self.accent_punct = 0
        self.uncertain_word = 0
        self.filler_word = 0
        self.notional_pos = 0
        self.functional_pos = 0
        self.syllables = 0
        self.word_freq = 0.0
        self.informal = 0
        self.complex_dep = 0
        self.pejorative = 0
        self.unkn_lemmas = 0
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
        self.words_len_rate = 0.0
        self.big_words_rate = 0.0
        self.animate_rate = 0.0
        self.first_rate = 0
        self.reflexive_rate = 0.0
        self.accent_punct_rate = 0.0
        self.notional_pos_rate = 0.0
        self.functional_pos_rate = 0.0
        self.syllables_rate = 0.0
        self.word_freq_rate = 0.0
        self.informal_rate = 0.0
        self.complex_dep_rate = 0.0
        self.pejorative_rate = 0.0
        self.unkn_lemmas_rate = 0.0
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
        self.pos_corpus = ''
        self.text_lemmas = ''

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
            self.notional_pos_rate = self.notional_pos / self.token_cnt
            self.functional_pos_rate = self.functional_pos / self.token_cnt
            self.complex_dep_rate = self.complex_dep / self.token_cnt
        if self.sent_cnt > 0:
            self.punct_sent_rate = self.punct_cnt / self.sent_cnt
            self.token_per_sent = self.token_cnt / self.sent_cnt
        if self.word_cnt > 0:
            self.uniq_rate = self.uniq_cnt / self.word_cnt
            self.title_rate = self.title_cnt / self.word_cnt
            self.caps_rate = self.caps_cnt / self.word_cnt
            self.words_len_rate = self.words_len / self.word_cnt
            self.big_words_rate = self.big_word_cnt / self.word_cnt
            self.syllables_rate = self.syllables / self.word_cnt
            self.word_freq_rate = self.word_freq / self.word_cnt
            self.informal_rate = self.informal / self.word_cnt
            self.pejorative_rate = self.pejorative / self.word_cnt
            self.unkn_lemmas_rate = self.unkn_lemmas / self.word_cnt
        if self.PRON > 0:
            self.animate_rate = self.animate_prons / self.PRON
            self.first_rate = self.first_person_pron / self.PRON
            self.reflexive_rate = self.reflexive_prons / self.PRON
        if self.punct_cnt > 0:
            self.accent_punct_rate = self.accent_punct / self.punct_cnt

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
                self.words_len,
                self.big_word_cnt,
                self.animate_prons,
                self.first_person_pron,
                self.reflexive_prons,
                self.accent_punct,
                self.uncertain_word,
                self.filler_word,
                self.notional_pos,
                self.functional_pos,
                self.syllables,
                self.word_freq,
                self.informal,
                self.complex_dep,
                self.pejorative,
                self.unkn_lemmas,
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
                self.words_len_rate,
                self.big_words_rate,
                self.animate_rate,
                self.first_rate,
                self.reflexive_rate,
                self.accent_punct_rate,
                self.notional_pos_rate,
                self.functional_pos_rate,
                self.syllables_rate,
                self.word_freq_rate,
                self.informal_rate,
                self.complex_dep_rate,
                self.pejorative_rate,
                self.unkn_lemmas_rate,
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
                self.INTJ,
                self.pos_corpus,
                self.text_lemmas]


def count_syl(word):
    count = 0
    vowels = 'aeiouy'
    word = word.strip(".:;?!")
    if word[0] in vowels:
        count += 1
    for index in range(1, len(word)):
        if word[index] in vowels and word[index-1] not in vowels:
            count += 1
    if word.endswith('e'):
        count -= 1
    if word.endswith('le'):
        count += 1
    if count == 0:
        count += 1
    return count


def make_lemma_set(filename):
    lemma_set = set([])
    with open(filename, 'r', encoding='utf-8') as f1:
        lines = f1.read().split('\n')
        for lemma in lines:
            lemma_set.add(lemma)
    lemma_set.remove('')
    return lemma_set


def prepare(text, nlp, lemmas_set):
    text = re.sub(r'[0-9]+', '999', text)
    text = re.sub(r'&nbsp;?|[\s_]+', ' ', text).strip(' _')
    doc = nlp(text)
    unique_lemmas = set()
    features = Features()
    lemma_text = ''
    pos_corpus = 'START '
    for token in doc:
        features.token_cnt += 1

        if token.is_sent_start:
            features.sent_cnt += 1
            pos_corpus += 'END START '

        features.title_cnt += token.is_title
        features.caps_cnt += token.is_upper
        features.number_cnt += token.is_digit
        features.notional_pos += 1 if token.pos_ in NOTION_POS else 0
        features.functional_pos += 1 if token.pos_ in FUNCTIONAL_POS else 0
        pos_corpus += '{} '.format(token.pos_)

        if token.is_punct:
            if token.string in SMILES:
                features.smiles_cnt += 1
            else:
                features.punct_cnt += 1
            if len(token.string) > 1 and re.match(r'[\?\!\.]', token.string):
                features.accent_punct += 1

        if token.is_alpha:
            features.syllables += count_syl(token.lower_)
            features.word_cnt += 1
            unique_lemmas.add(token.lemma_)
            tok_len = len(token)
            features.words_len += tok_len
            features.big_word_cnt += 1 if tok_len > 6 else 0
            features.filler_word += 1 if token.lower_ in FILLER_WORDS else 0
            features.informal += 1 if token.lower_ in INFORMAL_CONTR else 0
            features.word_freq += word_frequency(token.lower_, 'en')
            features.pejorative += 1 if token.lower_ in PEJORATIVE else 0
            lemma_text += '{} '.format(token.lemma_)

        if token.pos_ == 'PRON':
            features.first_person_pron += 1 if token.lower_ in FIRST_PERS else 0
            features.animate_prons += 1 if token.lower_ in ANIMATE_PRON else 0
            features.reflexive_prons += 1 if token.lower_.endswith(('self', 'lves')) else 0
        elif token.pos_ == 'ADV':
            if token.lower_ in UNCERTAN_WORDS:
                features.uncertain_word += 1

        if token.pos_ in NOTION_POS:
            features.unkn_lemmas += 0 if token.lemma_ in lemmas_set else 1

        if token.dep_ in COMPLEX_DEPS:
            features.complex_dep += 1

        features.pos_dict[token.pos_] += 1
    features.pos_corpus = pos_corpus
    features.text_lemmas = lemma_text
    features.uniq_cnt = len(unique_lemmas)
    features.set_pos_values()
    features.calculate_rates()
    return features.to_array()

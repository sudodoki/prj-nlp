import re, logging
from polyglot.text import Text
from zipfile import ZipFile
from gensim.models import Word2Vec
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

class RawTextDoc():
    '''
    Streams TaggedDocuments from specified zip
    '''
    zf = None
    files = None

    def __init__(self, zf):
        self.zf = zf
        with ZipFile(zf) as zipf:
            self.files = zipf.namelist()

    def __len__(self):
        return len(self.files)

    def __iter__(self):
        with ZipFile(self.zf) as zf:
            for fname in self.files:
                with zf.open(fname) as f:
                    paragraphs = ''.join([ch for ch in f.read().decode()
                                          if ch.isprintable()]
                                        ).split('\n')
                    sents = []
                    for p in paragraphs:
                        if len(p) > 0:
                            p = Text(p)
                            sents += [[re.sub('[^A-Za-zА-Яа-яЄєІіЇїҐ\-5\']', '',
                                              re.sub('\d+', '5',
                                                     re.sub('-‑', '-',
                                                            re.sub("[‘’`']", "'",
                                                                   w))))
                                       for w in sent.words]
                                      for sent in p.sentences if len(sent.words) > 0]
                    sents = [list(filter(lambda w: len(w) > 0, sent))
                             for sent in sents]
                    sents = list(filter(lambda sent: len(sent) > 0, sents))
                    for sent in sents:
                        yield sent

docs = RawTextDoc('../nolemm_htmls.zip')

w2v = Word2Vec(sentences=docs, size=50, window=5,
               max_vocab_size=5000000, seed=54548,
               workers=5, iter=10, batch_words=15000)

w2v.save('w2v_sent_5dim_5win_5Mwaxvocab_15Kbatch_10epoch.w2v')
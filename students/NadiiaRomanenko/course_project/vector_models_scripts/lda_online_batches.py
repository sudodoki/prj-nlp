import logging, re, glob, os, tempfile
from zipfile import ZipFile
from tqdm import tqdm
from gensim.corpora import Dictionary, MmCorpus
from gensim.models import LdaModel
from random import shuffle
from os.path import join as pjoin
from time import time

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

class DocStream():
    path = None
    files = None
    done = None
    
    def __init__(self, path):
        self.path = path
        with open('lda_done.txt', 'r') as f:
            self.done = f.read().split('\n')
        with ZipFile(self.path) as zf:
            files = zf.namelist()
            files = list(set(files) - set(self.done))
            shuffle(files)
            self.files = files[:20000]
            
            
    def __len__(self):
        return len(self.files)
        
    def __iter__(self):
        with ZipFile(self.path) as zf:
            for fname in self.files:
                doc = zf.open(fname).read().decode()
                tokens = list(filter(lambda w: '__' in w \
                                     and not re.search(
                                         '__[(PNCT)(PREP)(CONJ)(PRCL)(NPRO)(INTJ)(pnct)(prep)(conj)(prcl)(npro)(intj)]', w),
                                     re.findall('[\w_]+', doc)))
                if len(tokens) > 0:
                    yield tokens

    def save_done(self):
        with open('lda_done.txt', 'w') as f:
            f.write('\n'.join(set(self.done).union(set(self.files))))
            logging.info('UPDATED lda_done.txt')


class BoWStream():
    docs = None
    
    def __init__(self, docs):
        self.docs = docs
        
    def __len__(self):
        return len(docs)
        
    def __iter__(self):
        for doc in docs:
            yield dictionary.doc2bow(doc)


dictionary = Dictionary.load('pos_lemmatized_nofunctors.dict')

while True:
    docs = DocStream('../pos_lemmatized.zip')
    corpus = BoWStream(docs)
    with tempfile.TemporaryDirectory() as tempdir:
        timemark = int(time())
        MmCorpus.serialize(pjoin(tempdir, f'temp_corpus_lda_{timemark}.mm'), corpus)
        del corpus
        
        corpus = MmCorpus(pjoin(tempdir, f'temp_corpus_lda_{timemark}.mm'))
        
        lda = LdaModel.load('../vectors/lda050418_1000dim_15pass_100iter_10offset_0.7lr.lda')
        
        lda.update(corpus)
        
#         lda = LdaModel(corpus,
#                        num_topics=1000,
#                        id2word=dictionary,
#                        chunksize=2000,
#                        passes=15,
#                        update_every=1,
#                        alpha='symmetric',
#                        eta=None,
#                        decay=0.7,
#                        offset=10.0,
#                        eval_every=10,
#                        iterations=100,
#                        gamma_threshold=0.001,
#                        minimum_probability=0.01,
#                        random_state=4561,
#                        ns_conf=None,
#                        minimum_phi_value=0.01)

        lda.save('../vectors/lda050418_1000dim_15pass_100iter_10offset_0.7lr.lda')
        docs.save_done()
        del docs
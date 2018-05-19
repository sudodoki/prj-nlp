import logging, re, pdb
from zipfile import ZipFile
from gensim.corpora import Dictionary, MmCorpus
from gensim.models import TfidfModel, KeyedVectors, LdaModel
from gensim.matutils import corpus2dense, corpus2csc
from polyglot.text import Text
import numpy as np
from scipy import sparse

from .preprocessing import *

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)


class TfIdfGensimVectoriser():
    tfidf = None
    dictionary = None
    lemmatize = False
    
    def __init__(self, tfidf_path, dictionary, lemmatize=False):
        self.tfidf = TfidfModel.load(tfidf_path)
        self.dictionary = Dictionary.load(dictionary)
        self.lemmatize = lemmatize
        
    def fit(self):
        pass
    
    def transform_one(self, sentence):
        if isinstance(sentence, str):
            sentence = [str(w) for w in Text(sentence).words]
        elif isinstance(sentence, list):
            pass
        else:
            raise TypeError(f'Input document must be str or list. Received {type(sentence)} instead')
        if self.lemmatize:
            lang = get_lang(sentence)
            if lang:
                sentence = [lemmatize_word(w, lang) for w in sentence]
                
        sentence_tfidf = self.tfidf[self.dictionary.doc2bow(sentence)]
        
        return sentence_tfidf
    
    
    def transform(self, sent_list):
        sents_transformed = [self.transform_one(sent) for sent in sent_list]
        return corpus2csc(sents_transformed,
                          num_terms=len(self.dictionary),
                          num_docs=len(sent_list)
                         ).transpose()
        

class TfIdfD2vVectoriser():
    w2v = None
    tfidf = None
    dictionary = None
    lemmatize = False
    
    def __init__(self, vec_path, tfidf_path, dictionary, lemmatize=False):
        self.w2v = KeyedVectors.load(vec_path)
        self.tfidf = TfidfModel.load(tfidf_path)
        self.dictionary = Dictionary.load(dictionary)
        self.lemmatize = lemmatize
        
    def fit(self):
        pass
    
    def transform_one(self, sentence):
        if isinstance(sentence, str):
            sentence = [str(w) for w in Text(sentence).words]
        elif isinstance(sentence, list):
            pass
        else:
            raise TypeError(f'Input document must be str or list. Received {type(sentence)} instead')
        if self.lemmatize:
            lang = get_lang(sentence)
            if lang:
                sentence = [lemmatize_word(w, lang) for w in sentence]
                
        sentence_tfidf = self.tfidf[self.dictionary.doc2bow(sentence)]
        
        sentence_tfidf = np.array([self.w2v.wv[self.dictionary[i].split('__')[0]] * weight
                                            for i, weight in sentence_tfidf
                                            if self.dictionary[i].split('__')[0] in self.w2v.wv.vocab])
        
        try:
            return np.mean(sentence_tfidf, axis=0)
        except:
            pdb.set_trace()
    
    def transform(self, sent_list):
        
        sents_transformed = [self.transform_one(sent) for sent in sent_list]
        sents_transformed = [sent if isinstance(sent, np.ndarray) else np.zeros((50))
                             for sent in sents_transformed]

        return sparse.csr_matrix(sents_transformed)

    
class LdaVectorizer():
    lda = None,
    tfidf = None,
    dictionary = None
    lemmatize = False
    
    def __init__(self, lda_path, tfidf_path, dictionary, lemmatize=False):
        self.lda = LdaModel.load(lda_path)
        self.tfidf = TfidfModel.load(tfidf_path)
        self.lemmatize = lemmatize
        self.dictionary = Dictionary.load(dictionary)
        
    def fit(self):
        pass
    
    def transform_one(self, sentence):
        if isinstance(sentence, str):
            sentence = [str(w) for w in Text(sentence).words]
        elif isinstance(sentence, list):
            pass
        else:
            raise TypeError(f'Input document must be str or list. Received {type(sentence)} instead')
        if self.lemmatize:
            lang = get_lang(sentence)
            if lang:
                sentence = [lemmatize_word(w, lang) for w in sentence]
                
        sentence_tfidf = self.tfidf[self.dictionary.doc2bow(sentence)]
                
        return self.lda[sentence_tfidf]
                
    def transform(self, sent_list):
        sents_transformed = [self.transform_one(sent) for sent in sent_list]
        return corpus2csc(sents_transformed,
                          num_terms=1000, # No topics in LDA
                          num_docs=len(sent_list)
                         ).transpose()
    
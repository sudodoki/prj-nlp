import numpy as np
import pandas as pd
import spacy
import random
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report
from sklearn.utils import resample
from sklearn.preprocessing import StandardScaler
from nltk.corpus import wordnet as wn
from nltk.corpus import wordnet_ic

def penn_to_wn(tag):
    #Convert between the PennTreebank tags to simple Wordnet tags
    if tag.startswith('J'):
        return wn.ADJ
    elif tag.startswith('N'):
        return wn.NOUN
    elif tag.startswith('R'):
        return wn.ADV
    elif tag.startswith('V'):
        return wn.VERB
    return None

def get_synset(token): 
    wn_tag = penn_to_wn(token.tag_)
    
    if not wn_tag:
        return None

    return wn.synsets(token.lemma_, pos=wn_tag)

def add_to_sim(total, sim):
    for i in range(0, len(total)):
        total[i] += sim[i]

def sim_divide(total, cnt):
    if cnt == 0:
        total = [0,0,0,0,0,0]
    else:
        for i in range(0, len(total)):
            total[i] = round(total[i] / cnt, 3)

def calc_similarity(synsets1, synsets2):
    total_sim, cnt, exact_cnt = [0,0,0,0,0,0], 0, 0

    for synset2 in synsets2:
        sim = [0,0,0,0,0,0]
        cnt += 1        
        for synset1 in synsets1:
            for s1 in synset1:
                for s2 in synset2:
                    if s1.pos() == s2.pos():
                        try:
                            tmp_sim = s2.path_similarity(s1)
                        except:
                            tmp_sim = 0
                        sim[0] = max(sim[0], tmp_sim if tmp_sim else 0)
                        try:
                            tmp_sim = s2.lch_similarity(s1)
                        except:
                            tmp_sim = 0                            
                        sim[1] = max(sim[1], tmp_sim if tmp_sim else 0)
                        try:
                            tmp_sim = s2.wup_similarity(s1)
                        except:
                            tmp_sim = 0                            
                        sim[2] = max(sim[2], tmp_sim if tmp_sim else 0)
                        try:
                            tmp_sim = s2.res_similarity(s1, brown_ic)
                        except:
                            tmp_sim = 0                            
                        sim[3] = max(sim[3], tmp_sim if tmp_sim else 0)
                        try:
                            tmp_sim = s2.jcn_similarity(s1, brown_ic)
                        except:
                            tmp_sim = 0                            
                        sim[4] = max(sim[4], tmp_sim if tmp_sim else 0)
                        try:
                            tmp_sim = s2.lin_similarity(s1, brown_ic)
                        except:
                            tmp_sim = 0                            
                        sim[5] = max(sim[5], tmp_sim if tmp_sim else 0)                                                
        if sim[0] == 1.0:
            exact_cnt += 1            
        add_to_sim(total_sim, sim)
    sim_divide(total_sim, cnt)
    total_sim.append(exact_cnt)
    return np.array(total_sim)

def feature_extraction(tokens1, tokens2):    
    synsets1 = [get_synset(x) for x in tokens1]
    synsets2 = [get_synset(x) for x in tokens2]

    synsets1 = [x for x in synsets1 if x is not None]
    synsets2 = [x for x in synsets2 if x is not None]    

    return calc_similarity(synsets1, synsets2)  

def spacy_batch_tokenize(data):
    docs = []
    for doc in nlp.pipe(data['sentence1'].values, n_threads=32, batch_size=256):
        docs.append(doc)
    data['tokens1'] = pd.Series(docs, index=data.index)
    
    docs = []
    for doc in nlp.pipe(data['sentence2'].values, n_threads=32, batch_size=256):
        docs.append(doc)
    data['tokens2'] = pd.Series(docs, index=data.index)
    return data

def prepare_data(data):
    processed = 0
    data = spacy_batch_tokenize(data)
    features, labels = [], []
    for index, row in data.iterrows():
        features.append(feature_extraction(row['tokens1'], row['tokens2']))
        labels.append(row['gold_label'])        
        processed += 1
        if processed % 100 == 0:
            print("Processed:", processed)
    print("Processed:", processed)
    return np.array(features), labels

def vectorize_data(data, vectorizer, imputer, scaler):
    return scaler.transofrm(imputer.transform(vectorizer.transform(data)))

nlp = spacy.load('en')
brown_ic = wordnet_ic.ic('ic-brown.dat')

train_data = pd.read_json('_snli_1.0_train.jsonl', lines=True, encoding='latin1')[['sentence1', 'sentence2', 'gold_label']]
train_data = train_data[train_data['gold_label'] != '-']

test_data = pd.read_json('_snli_1.0_test.jsonl', lines=True, encoding='latin1')[['sentence1', 'sentence2', 'gold_label']]
test_data = test_data[test_data['gold_label'] != '-']

random_state = random.randint(1,100)
n_samples = 1000

contr_train = resample(train_data[train_data['gold_label'] == 'contradiction'], replace=False, n_samples=n_samples, random_state=random_state)
entail_train = resample(train_data[train_data['gold_label'] == 'entailment'], replace=False, n_samples=n_samples, random_state=random_state)
neutral_train = resample(train_data[train_data['gold_label'] == 'neutral'], replace=False, n_samples=n_samples, random_state=random_state)

train_data = pd.concat([contr_train, entail_train, neutral_train])
train_features, train_labels = prepare_data(train_data)
test_features, test_labels = prepare_data(test_data)

scaler = StandardScaler()
scaler.fit(np.concatenate([train_features,test_features]))

train_features_vectorized = scaler.transform(train_features)
test_features_vectorized = scaler.transform(test_features)

print("Logistic Regression Classifier")
lrc = LogisticRegression(random_state=random_state, multi_class='multinomial', solver='saga', C=0.05, n_jobs=-1, max_iter=1000)
lrc.fit(train_features_vectorized, train_labels)
predicted = lrc.predict(test_features_vectorized)
print(classification_report(test_labels, predicted, digits=3))

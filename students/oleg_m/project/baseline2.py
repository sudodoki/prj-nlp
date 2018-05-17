import pandas as pd
import numpy as np
import time
import re
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import accuracy_score, precision_score, recall_score, confusion_matrix, \
    precision_recall_fscore_support, f1_score, roc_auc_score, roc_curve, auc
from sklearn.model_selection import GridSearchCV

DATA_DIR = '/Users/admin/edu/NLP/practical_NLP_course/data/'
BLOGS_PREP_EN_FILE = 'blog_authors_preped.csv.gzip'


def clean_text(text):
    text = re.sub(r'(?:https?://|www\.)[^\s]+', '', text)
    text = re.sub(r'[0-9]+', ' 999 ', text)
    text = re.sub(r'[\s_]+', ' ', text)
    text = re.sub(r'(\w)\1{3,}', r'\1', text)
    if len(text) > 20:
        text = text[:20]
    return text.strip()


data = pd.read_csv(DATA_DIR + BLOGS_PREP_EN_FILE, compression='gzip')
data['text_cleaned'] = data['text'].apply(clean_text)
data.drop(['text'], axis=1, inplace=True)

X_train, X_test, y_train, y_test = train_test_split(data[['text_cleaned', 'word_count', 'char_length']],
                                                    data['age+sex'],
                                                    test_size=0.25,
                                                    random_state=249)

print('PREPARE BOW', time.strftime('%H:%M:%S', time.localtime()))
bow_transformer = CountVectorizer(min_df=8).fit(X_train.text_cleaned)
print('BOW SIZE:', len(bow_transformer.vocabulary_))

print('PREPARE BOW DF', time.strftime('%H:%M:%S', time.localtime()))
X_train_bow = pd.DataFrame(bow_transformer.transform(X_train['text_cleaned']).todense(),
                           columns=bow_transformer.get_feature_names(),
                           index=X_train.index)

# print('CONCAT DFs', time.strftime('%H:%M:%S', time.localtime()))
# X_train_basline = pd.concat([X_train_bow, X_train[['word_count', 'char_length']]], axis=1, join="inner")

print('STARTING BUILDING MODEL', time.strftime('%H:%M:%S', time.localtime()))

scoring = ['precision_macro', 'recall_macro', 'f1_macro', 'accuracy']
parameters = {'alpha': [0.001, 0.01, 0.1, 1.0]}
clf_temp = GridSearchCV(MultinomialNB(), parameters, cv=4, return_train_score=False, n_jobs=4,
                        scoring=scoring, refit=False)
clf_temp.fit(X_train_bow, y_train)

print('MODEL BUILD', time.time())
clf_temp_data = {re.sub('_macro','',k):v for k, v in clf_temp.cv_results_.items() if k.startswith('mean_test') or k.startswith('rank_')}
clf_temp_data['alpha'] = [0.001, 0.01, 0.1, 1.0]
print(pd.DataFrame(clf_temp_data))

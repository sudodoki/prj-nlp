import pandas as pd
import numpy as np
import time
import re
from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.naive_bayes import MultinomialNB
from sklearn.metrics import accuracy_score, precision_score, recall_score, confusion_matrix, \
    precision_recall_fscore_support, f1_score, roc_auc_score, roc_curve, auc

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
                                                    test_size=0.2,
                                                    random_state=249)

print('PREPARE BOW', time.strftime('%H:%M:%S', time.localtime()))
bow_transformer = CountVectorizer(min_df=7).fit(X_train.text_cleaned)
print('BOW SIZE:', len(bow_transformer.vocabulary_))

print('PREPARE BOW DF', time.strftime('%H:%M:%S', time.localtime()))
X_train_bow = pd.DataFrame(bow_transformer.transform(X_train['text_cleaned']).todense(),
                           columns=bow_transformer.get_feature_names(),
                           index=X_train.index)

print('CONCAT DFs', time.strftime('%H:%M:%S', time.localtime()))
X_train_basline = pd.concat([X_train_bow, X_train[['word_count', 'char_length']]], axis=1, join="inner")

print('STARTING BUILDING MODEL', time.strftime('%H:%M:%S', time.localtime()))
model = MultinomialNB(alpha=0.1)
model.fit(X_train_basline, y_train)
print('MODEL BUILD', time.time())
X_test_bow = pd.DataFrame(bow_transformer.transform(X_test['text_cleaned']).todense(),
                          columns=bow_transformer.get_feature_names(),
                          index=X_test.index)
X_test_basline = pd.concat([X_test_bow, X_test[['word_count', 'char_length']]], axis=1, join="inner")

predictions = model.predict(X_test_basline)
print('RESULTS:', time.strftime('%H:%M:%S', time.localtime()))
print('Accuracy:', accuracy_score(y_test, predictions))
print()
print('Precision weighted:', precision_score(y_test, predictions, average='weighted'))
print('Recall weighted:', recall_score(y_test, predictions, average='weighted'))
print('F1 score weighted:', f1_score(y_test, predictions, average='weighted'))
print()
print('Precision macro:', precision_score(y_test, predictions, average='macro'))
print('Recall macro:', recall_score(y_test, predictions, average='macro'))
print('F1 score macro:', f1_score(y_test, predictions, average='macro'))

conf_matrix_temp = confusion_matrix(y_test, predictions)
conf_matrix_baseline = pd.DataFrame(conf_matrix_temp)
print(conf_matrix_baseline)

supp_matrix_temp = [list(x) for x in precision_recall_fscore_support(y_test, predictions)]
supp_matrix_baseline = pd.DataFrame(supp_matrix_temp, index=['precision', 'recall', 'f1', 'total'])
print(supp_matrix_baseline)

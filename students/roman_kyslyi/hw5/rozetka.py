from collections import defaultdict, Counter
import pymorphy2
import requests
from bs4 import BeautifulSoup
import re
import numpy as np
from nltk.corpus import stopwords
from sklearn.ensemble import RandomForestClassifier
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics import precision_score, recall_score, accuracy_score
from spacy.lang.xx import MultiLanguage
import csv
import pandas as pd
from sklearn.model_selection import train_test_split


nlp = MultiLanguage()
morph = pymorphy2.MorphAnalyzer()
tokenizer = MultiLanguage().Defaults.create_tokenizer(nlp)
stop_words = set(stopwords.words('russian'))
POSITIVE = "Переваги:\xa0"
NEGATIVE = "Недоліки:\xa0"
FEEDBACK_FILE = "scrape_feedbacks.csv"
vocabulary_pr = {}


def get_data(url, counter, cls):
    url = url.format(counter)
    resp = requests.get(url)
    page = re.compile("page=(\d+)/").search(resp.url)
    if page:
        curr_page = int(page.groups()[0])
    else:
        curr_page = 1
    if curr_page != counter:
        print('END: final page reached:', counter-1)
        return
    soup = BeautifulSoup(resp.text, 'html5lib')
    for script in soup(["script", "style"]):
        script.decompose()  # rip it out
    data = soup.select(cls)
    if not data:
        print('END: no data:')
        return
    return data



def write_to_file(items_links):
    with open(FEEDBACK_FILE, "a") as fl:
        spamwriter = csv.writer(fl)
        for num, link in enumerate(items_links):
            last_review = None
            print("fetching {} out of {} products".format(num, len(items_links)))
            page_c = 1
            while True:
                print("fetching {}comments/page={}".format(link, page_c))
                reviews = get_data(link+"comments/page={}", page_c, '.pp-review-inner')
                if not reviews:
                    break
                if last_review == reviews[-1]:
                    print("END: duplicated content")
                    break
                last_review = reviews[-1]
                for review in reviews:
                    stars = review.find('span', {'class':'g-rating-stars-i'})
                    if stars:
                        texts = review.select(".pp-review-text-i")
                        advantage, drawback = None, None
                        for num, txt in enumerate([i.text.replace("\n", "") for i in texts]):
                            if num == 0:
                                text = txt
                            elif POSITIVE in txt:
                                advantage = txt.replace(POSITIVE, "")
                            elif NEGATIVE in txt:
                                drawback = txt.replace(NEGATIVE, "")
                        spamwriter.writerow([int(stars.attrs['content']), advantage, drawback, text])
                page_c += 1
    fl.close()

def bow(x_train, y_train):
    occurences = defaultdict(Counter)
    for text, cls in zip(x_train, y_train):
        for token in tokenizer(text):
            normal_form = morph.parse(token.text)[0].normal_form
            occurences[cls].update([normal_form])
    return occurences


def get_metrics(labels, predictions):
    precision = precision_score(labels, predictions, average='macro')
    recall = recall_score(labels, predictions, average='macro')
    Fscore = 2 * precision * recall / (precision + recall)
    accuracy = accuracy_score(labels, predictions)
    print("Precision: ", precision)
    print("Recall: ", recall)
    print("F-score: ", Fscore)
    print("Accuracy: ", accuracy)


def predict(vocabulary_pr, texts):
    prediction = []
    tone_val = [0.0001 for i in range(len(classes))]
    for text in texts:
        probs_stats = []
        for token in tokenizer(text):
            normal_form = morph.parse(token.text)[0].normal_form
            if not normal_form in stop_words:
                if normal_form in vocabulary_pr:
                    probs_stats.append(vocabulary_pr[normal_form])
                if normal_form in tone_dict:
                    tone_val_c = tone_val.copy()
                    probs_stats.append(tone_val_c)
        probs_stats = np.prod(np.array(probs_stats), axis=0)
        prediction.append(np.argmax(probs_stats) + 1)
    return prediction


def train():
    for word in vocabulary:
        if word not in stop_words:
            occurences_word_by_cls = {cls: occurences[cls][word] for cls in classes}
            occurences_word_total = sum(occurences_word_by_cls.values())
            probs = []
            for cls in classes:
                pr = ((occurences_word_by_cls[cls] / occurences_by_cls[cls]) * (
                occurences_by_cls[cls] / len(x_train))) / ((occurences_word_total) / (len(x_train)))
                if pr == 0:
                    pr = 0.0001
                elif pr >= 1:
                    pr = 0.9999
                probs.append(pr)
            vocabulary_pr[word] = probs


def test_metrics(y_train, x_train, y_test, x_test):
    print("TRAIN:")
    get_metrics(y_train.tolist(), predict(vocabulary_pr, x_train))
    print("TEST:")
    get_metrics(y_test.tolist(), predict(vocabulary_pr, x_test))



if __name__ == "__main__":
    '''items_links = []
    page_c = 1
    i = 0
    while i < 10:
        data = get_data("https://rozetka.com.ua/ua/palatki-i-aksessuary/c82412/page={}", page_c, '.g-i-tile-i-title a')
        if not data:
            page_c += 1
            continue
        items_links += [i.attrs["href"] for i in data]
        i += 1
    write_to_file(items_links)'''
    df = pd.read_csv(FEEDBACK_FILE)
    df = df.drop_duplicates()
    x = df["text"].astype(str) + " " + df["positive"].astype(str) + " " + df["negative"].astype(str)
    y = df["score"]
    x_train, x_test, y_train, y_test = train_test_split(x.tolist(), y, test_size=0.2)
    tone_dict = {}
    occurences = bow(x_train, y_train)
    classes = sorted(occurences.keys())
    occurences_by_cls = y_train.value_counts()
    vocabulary = set([item for sublist in [occurences[cls].keys() for cls in classes] for item in sublist])

    train()
    priors = y_train.value_counts(normalize=True).values
    vectorizer = TfidfVectorizer()
    train = vectorizer.fit_transform(x_train)
    test = vectorizer.transform(x_test)
    clf = RandomForestClassifier()
    clf.fit(train.toarray(), y_train)
    pred = clf.predict(test.toarray())
    print("TEST SET METRICS:")
    get_metrics(y_test.tolist(), pred)
    tone_dict = pd.read_csv("tone-dict-uk.tsv", delimiter='\t', names=["word", "mark"])
    # bad(1), neutral(2) and good(3):
    tone_mapping = {-2: 1, -1: 2, 0: 3, 1: 4, 2: 5}
    tone_dict["mark"] = tone_dict["mark"].map(tone_mapping)
    tone_dict = tone_dict.set_index('word').T.to_dict('int')['mark']
    test_metrics(y_train, x_train, y_test, x_test)
    prev_len = len(vocabulary_pr)
    #changed vocabulary length
    test_metrics(y_train, x_train, y_test, x_test)



'''
TEST SET METRICS:
Precision:  0.16
Recall:  0.25
F-score:  0.19512195121951217
Accuracy:  0.64
TRAIN:
Precision:  0.7951807228915663
Recall:  0.7866666666666667
F-score:  0.7909007819640499
Accuracy:  0.98
TEST:
Precision:  0.16
Recall:  0.25
F-score:  0.19512195121951217
Accuracy:  0.64
TRAIN:
Precision:  0.7951807228915663
Recall:  0.7866666666666667
F-score:  0.7909007819640499
Accuracy:  0.98
TEST:
Precision:  0.16
Recall:  0.25
F-score:  0.19512195121951217
Accuracy:  0.64


clear overfited :( need to parse more data
'''
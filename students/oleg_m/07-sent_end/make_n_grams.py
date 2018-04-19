import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction import DictVectorizer
# lko = ['lol', 'pop', 'duche', ',', 'kind']
# lko = ['lol ponc', 'pop ionsa', 'duche huche', 'hi ,', 'very kind']
# g = CountVectorizer(ngram_range=(1, 3))

# df = pd.read_csv('lucene_corpus3.csv')
# bow = CountVectorizer(ngram_range=(1, 3)).fit(df['threegram_before'])
# X_text_tfidf = pd.DataFrame(bow.transform(df['threegram_before']).todense(),
#                             columns=bow.get_feature_names())
# print(g)


lko = ['I want eat', 'wolf is strange', 'no one here']
bow = CountVectorizer(ngram_range=(1, 3)).fit(lko)
print(bow.vocabulary)
koka = ['no one lives forever', 'strange']
f = pd.DataFrame(bow.transform(koka).todense(), columns=bow.get_feature_names())
print(f)

pos_window = [
    {
        'word-2': 'the',
        'pos-2': 'DT',
        'word-1': 'cat',
        'pos-1': 'NN',
        'word+1': 'on',
        'pos+1': 'PP'
    },
{
        'word-2': 'cat',
        'pos-2': 'NN',
        'word-1': 'sit',
        'pos-1': 'VERB',
        'word+1': 'the',
        'pos+1': 'DT'
    }
]
v = DictVectorizer()
v.fit_transform(pos_window)
v.get_feature_names()
print(v)

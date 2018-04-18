import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer

# lko = ['lol', 'pop', 'duche', ',', 'kind']
# lko = ['lol ponc', 'pop ionsa', 'duche huche', 'hi ,', 'very kind']
# g = CountVectorizer(ngram_range=(1, 3))

df = pd.read_csv('lucene_corpus3.csv')
bow = CountVectorizer(ngram_range=(1, 3)).fit(df['threegram_before'])
X_text_tfidf = pd.DataFrame(bow.transform(df['threegram_before']).todense(),
                            columns=bow.get_feature_names())
print(g)

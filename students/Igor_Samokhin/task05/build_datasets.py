import pandas as pd

# stopwords
with open('stopwords_uk.txt', 'r') as f:
    stopwords = [w.strip() for w in f.readlines()]
stopwords.remove('не')

# start by importing data
df = pd.read_csv('ukr_reviews.csv.gz', compression='gzip')

# make columns indicating positive or negative sentiment 
df['sent'] = df['rating'].apply(lambda x: 'pos' if x>3 else 'neg')
df['sent2'] = df['rating'].replace({5: 'pos', 4: 'neu', 3: 'neg', 2: 'neg', 1: 'neg'})
df['sent3'] = df['rating'].replace({5: +2, 4: +1, 3: -1, 2: -2, 1: -3})


def build_datasets(df, random_state=1, balanced=False):
    """
    A function to build train and test datasets for
    sentiment analysis of positive and negative reviews,
    including ability to construct a balanced train set
    because of small amount of negative reviews
    """
    if balanced:
        n_neg = len(df[df['sent']=='neg'])
        # make number of positive equal number of negative
        new_pos = df[df['sent']=='pos'].sample(n_neg, random_state=random_state)
        balanced_df = pd.concat([new_pos, df[df['sent']=='neg']])
        train = balanced_df.sample(int(0.5*len(balanced_df)), 
                                   random_state=random_state)
        # for test set, use the same proportions as in initial df
        pos_prop = len(df[df['sent']=='pos'])/len(df[df['sent']=='neg'])
        test_neg = df[(~df.index.isin(train.index)) & (df['sent']=='neg')]
        test_pos = (df[(~df.index.isin(train.index)) & (df['sent']=='pos')]
                   .sample(int(len(test_neg)*pos_prop), random_state=random_state))
        test = pd.concat([test_pos, test_neg]).sample(frac=1, random_state=random_state)
        return train, test
    else:
        train = df.sample(frac=0.7, random_state=random_state)
        test = df[~df.index.isin(train.index)]
        return train, test


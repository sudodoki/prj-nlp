import re
import pandas as pd
from sklearn.metrics import classification_report
from students.oleg_m.fact_check import checker


def preapare_evaluation(zip_file):
    """
    Prepare df for evaluating
    1. Get 50 random lines from db and assign label as True
    2. Get 50 random lines and mixes title and author, so label is false
    :param zip_file: zip archive with csv file
    :return: title, author, label
    """
    df = pd.read_csv(zip_file, header=0, compression='zip')
    # assign true labels
    sample_true = df.sample(50)[['Title', 'Author']]
    sample_true['y'] = 1
    # prepare false labels
    sample_false = df.sample(50)[['Title', 'Author']]
    sample_false.rename(columns={'Author': 'old_author'}, inplace=True)
    sample_false.reset_index(inplace=True, drop=True)
    false_authors = df.sample(50).reset_index(drop=True)[['Author']]
    sample_false = pd.concat([sample_false, false_authors], axis=1, join_axes=[sample_false.index])
    sample_false = sample_false[sample_false['old_author'] != sample_false['Author']]
    sample_false.drop(['old_author'], axis=1, inplace=True)
    sample_false['y'] = 0
    return pd.concat([sample_true, sample_false])


def check_row(row):
    """
    makes verification if the book is written by the author
    :param row: row of df
    :return: label (True, False) or nan if there is no such writer in Wiki
    """
    title = re.sub('\(.+\)', '', row['Title'])
    is_book = checker.compare_book(row['Author'], title)
    print('book of author:', is_book)
    print()
    return is_book


def evaluate_df(data):
    """
    prepare evaluation of the algorithm
    :param data: test df
    """
    data['y_pred'] = data.apply(check_row, axis=1)
    data = data[data['y_pred'].notnull()]
    y_true = data['y'].tolist()
    y_pred = data['y_pred'].tolist()
    print(classification_report(y_true, y_pred))


# df = preapare_evaluation('files/books.zip')
# df.to_csv('files/evaluate_temp.csv', index=False)
df = pd.read_csv('files/test_books.csv', header=0)
df.drop(['Category_id', 'Category_name'], axis=1, inplace=True)
evaluate_df(df)

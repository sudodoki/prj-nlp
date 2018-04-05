import os
import argparse
import re
import multiprocessing as mp

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt

from readability import Document
from langdetect import detect
from langdetect.lang_detect_exception import LangDetectException


TAG_RE = re.compile(r'<[^>]+>')


def is_valid_file(arg):
    """ Auxiliary function to make sure input file path exists.
    """
    if not os.path.exists(arg) or os.path.isdir(arg):
        raise argparse.ArgumentTypeError("Path {} doesn't exist or it is not a file".format(arg))
    return arg


def extract_title_and_summary(content):
    doc = Document(content)
    title = doc.title()
    try:
        lang = detect(title)
    except LangDetectException:
        lang = 'unknown'
    s = TAG_RE.sub('', doc.summary())
    s = ' '.join([x for x in s.split() if x.strip() != ''])
    return title, lang, s


def compute(df):
    df['data_fields'] = df['content'].apply(lambda x: extract_title_and_summary(x))
    df['title'] = df['data_fields'].apply(lambda x: x[0])
    df['lang'] = df['data_fields'].apply(lambda x: x[1])
    df['words'] = df['data_fields'].apply(lambda x: x[2])

    del df['content']
    del df['data_fields']
    return df


def main(input_file, output_file):
    df = pd.read_csv(input_file)

    c = mp.cpu_count()
    pool = mp.Pool(c)
    pool_results = pool.map(compute, np.array_split(df, c))
    result_df = pd.concat(pool_results)
    pool.close()
    pool.join()

    result_df.to_csv(output_file, index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='CS Wiktionary parser')
    parser.add_argument('-i', dest='input_file', type=is_valid_file,
                        help='Input csv file from Spark')
    parser.add_argument('-o', dest='output_file',
                        help='Output file with language/title/text detected')

    args = parser.parse_args()

    if os.path.exists(args.output_file):
        os.remove(args.output_file)

    main(args.input_file, args.output_file)
import os
import argparse
import json
import re

import pandas as pd
from bs4 import BeautifulSoup


def get_db(input_db):
    with open(input_db, 'r') as f:
        content = json.loads(f.read())

    db = dict()
    for _, data in content.items():
        url = data['dbpedia_urls']
        entity_name = re.search('^.+\/(.+)$', url).group(1)
        cast = data['cast']
        db[entity_name] = cast
    return db


def extract_and_save_paragraphs(input_dir, input_db, output_dir):
    db = get_db(input_db)

    html_files = os.listdir(input_dir)
    for h in html_files:
        name = h[:-5]
        cast = db.get(name, [])

        if not cast:
            continue

        with open(os.path.join(input_dir, h), 'r') as f:
            text = f.read()
        soup = BeautifulSoup(text, 'lxml')
        body = soup.find('div', class_='mw-parser-output')

        # get raw text
        paragraphs = []
        for p in body.select('p'):
            paragraph = p.getText().strip()
            if paragraph:
                paragraphs.append(paragraph)

        new_file_name = '{}.txt'.format(name)

        # filter only preset casts
        p = ' '.join(paragraphs)
        new_cast = [c for c in cast if c in p]

        print('Writing {} paragraphs to {}'.format(len(paragraphs), new_file_name))
        df = pd.DataFrame({'paragraph': [' '.join(paragraphs)], 'cast': [','.join(new_cast)]})
        df.to_csv(os.path.join(output_dir, new_file_name), index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Fetch raw wiki pages')
    parser.add_argument('-i', dest='input_dir', help='Input fir with html files')
    parser.add_argument('-db', dest='input_db', help='Input json file with movies database')
    parser.add_argument('-o', dest='output_dir', help='Output dir, where to save raw data')

    args = parser.parse_args()

    if not os.path.exists(args.output_dir):
        os.mkdir(args.output_dir)

    extract_and_save_paragraphs(args.input_dir, args.input_db, args.output_dir)

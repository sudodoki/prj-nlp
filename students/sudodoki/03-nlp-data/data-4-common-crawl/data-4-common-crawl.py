# coding: utf-8


# aws s3 cp s3://commoncrawl/crawl-data/CC-NEWS/2016/09/CC-NEWS-20160928074341-00001.warc.gz input.warc.gz --no-sign-request

from warcio.archiveiterator import ArchiveIterator
import gzip
import pandas as pd
from bs4 import BeautifulSoup
from sklearn.feature_extraction.text import CountVectorizer
import urllib
import numpy as np
import matplotlib.pyplot as plt
from wordcloud import WordCloud
get_ipython().run_line_magic('matplotlib', 'inline')

def get_text_from(html):
    soup = BeautifulSoup(html, "lxml")

    # kill all script and style elements
    for script in soup(["script", "style"]):
        script.decompose()

    # get text
    text = soup.get_text()

    # break into lines and remove leading and trailing space on each
    lines = (line.strip() for line in text.splitlines())
    # break multi-headlines into a line each
    chunks = (phrase.strip() for line in lines for phrase in line.split("  "))
    # drop blank lines
    text = '\n'.join(chunk for chunk in chunks if chunk)
    return text

i = 0
cleaned_data = pd.DataFrame(columns=list(["uri", "text"]))
with gzip.open("input.warc.gz") as stream:
    for record in ArchiveIterator(stream):
        if record.content_type == "application/warc-fields":
            continue
        i += 1
        if i % 1000 == 1: 
            print('.')
        text = get_text_from(record.raw_stream.read())
        uri = record.rec_headers.get_header('WARC-Target-URI')
        cleaned_data = cleaned_data.append([{ "uri": uri, 'text': text }])
print("Done")
# Further analysis in python notebook

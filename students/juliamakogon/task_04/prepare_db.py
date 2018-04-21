import os
import glob
from bs4 import BeautifulSoup
import re
import requests
from readability.readability import Document
import sys


def prepareMovieTitle(s):
    film = s
    end = len(film) - 4
    year = '0'
    match = re.search(r'\((\d{4}).* film\)', film)
    if match:
        year = match[1]
        end = match.start(0)
    match = re.search(r'"(.+)\(film\)"', film)
    if match:
        end = match.end(1)
    film = film[1:end].strip()
    return (film, year)

def filenameForMovie(name):
    return re.sub(r'[\s:;.\'\"\(\)\?\!]', '_', re.findall('"(.+)"', name)[0]) + '.txt'


def getTextFromHtml(path, name, url):
    try:
        fname = os.path.join(path, name)
        if os.path.isfile(fname):
            return

        print(fname)              
        r = requests.get(url)
        r.raise_for_status()
        summary = Document(r.content).summary()
        soup_wiki = BeautifulSoup(summary)
        text = soup_wiki.text.strip()
        if text:
            with open(fname, 'w', encoding='utf-8') as f:
                f.write(text)
    except Exception as e:
        print("Failed to save a page: {}.\nException: {}".format(url, e))    


def prepareDbPos(path, inputdb_html, outputdb_csv):
    with open(inputdb_html, 'r', encoding='utf-8') as fp:
        soup = BeautifulSoup(fp)

    table = soup.find('table')
    rows = table.find_all('tr')
    I_MOVIE = 1 # movieLabel
    I_WIKI = 2 # wiki
    I_DATE = 3
    I_YEAR_CAT = 4
    with open(outputdb_csv, 'w', encoding="utf-8") as f:
        for i in range(1, min(len(rows), 200000)) :
            td = rows[i].find_all("td")
            (film, year) = prepareMovieTitle(td[I_MOVIE].get_text())
            if year == '0':
                match = re.findall(r'\d{4}', td[I_DATE].get_text())
                if match:
                    year = match[0]
            if year =='0':
                match = re.findall(r'\d{4}', td[I_YEAR_CAT].get_text())
                if match:
                    year = match[0]               
            f.write('{}\t{}\n'.format(film, year))
            getTextFromHtml(path, filenameForMovie(td[I_MOVIE].get_text()), td[I_WIKI].get_text())  


def prepareNeg(path, input_html):
    with open(input_html, 'r') as fp:
        soup = BeautifulSoup(fp)

    table = soup.find('table')
    rows = table.find_all('tr')
    I_MOVIE = 1 # movieLabel
    I_WIKI = 2 # wiki
    for i in range(1, min(len(rows), 200000)) :
        td = rows[i].find_all("td")
        getTextFromHtml(path, filenameForMovie(td[I_MOVIE].get_text()), td[I_WIKI].get_text())    

#path = sys.argv[1]
dirpath = os.getcwd()
path = os.path.join(dirpath, r'testset')
pos_path = os.path.join(path, r'pos')
neg_path = os.path.join(path, r'neg')
print(pos_path)
print(neg_path)
prepareDbPos(pos_path, "cinderella_sparql.html", "cinderella_db.csv")
prepareNeg(neg_path, "hippi_sparql.html")
prepareNeg(neg_path, "false_pos_sparql.html")

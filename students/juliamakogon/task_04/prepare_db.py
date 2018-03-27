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
    year = 0
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
        with open(fname, 'w', encoding='utf-8') as f:
            f.write(soup_wiki.text)

    except Exception as e:
        print("Failed to save a page: {}.\nException: {}".format(url, e))    


def prepareDbPos(path, inputdb_html, outputdb_csv):
    with open(inputdb_html, 'r') as fp:
        soup = BeautifulSoup(fp)

    table = soup.find('table')
    rows = table.find_all('tr')
    I_MOVIE = 2 # movieLabel
    I_CAT = 3 # catLabel
    I_WIKI = 4 # wiki
    with open(outputdb_csv, 'w', encoding="utf-8") as f:
        for i in range(1, min(len(rows), 200000)) :
            td = rows[i].find_all("td")
            fairytale = td[I_CAT].get_text()
            match = re.findall(r'"Films based on (.+)"', fairytale)
            if match:
                fairytale = match[0]
            (film, year) = prepareMovieTitle(td[I_MOVIE].get_text())
            f.write('"{}"\t{}\t"{}"\n'.format(film, year, fairytale))
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

path = sys.argv[1]
#path = r'C:\work\jul\prj-nlp\students\juliamakogon\task_04\wiki_testset'
pos_path = os.path.join(path, r'pos')
neg_path = os.path.join(path, r'neg')
print(pos_path)
print(neg_path)
prepareDbPos(pos_path, "fairytale_sparql.html", "fairytale_db.csv")
prepareNeg(neg_path, "hippi_sparql.html")
 

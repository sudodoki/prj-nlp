import os
import glob
from bs4 import BeautifulSoup
import re

with open("fairytale_sparql.html") as fp:
    soup = BeautifulSoup(fp)

table = soup.find('table')
rows = table.find_all('tr')
I_MOVIE = 2 # movieLabel
I_CAT = 3 # catLabel
I_WIKI = 4 # wiki
with open("fairytale_db.csv", 'w', encoding="utf-8") as f:
    for i in range(1, min(len(rows), 100000)) :
        td = rows[i].find_all("td")
        print(td[I_MOVIE].get_text(),  td[I_CAT].get_text(), td[I_WIKI].get_text())
        film = td[I_MOVIE].get_text()
        end = len(film) - 4
        fairytale = td[I_CAT].get_text()
        match = re.findall(r'"Films based on (.+)"', fairytale)
        if match:
            fairytale = match[0]
        year = 0
        match = re.search(r'\((\d{4}).* film\)', film)
        if match:
            year = match[1]
            end = match.start(0)
        match = re.search(r'"(.+)\(film\)"', film)
        if match:
            end = match.end(1)
        film = film[1:end].strip()
        print(film,  year, fairytale, td[I_WIKI].get_text())
        f.write('"{}"\t{}\t"{}"\n'.format(film, year, fairytale))


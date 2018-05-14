import spacy
import re
import numpy as np
import requests
from bs4 import BeautifulSoup
from difflib import SequenceMatcher
import html

headers = requests.utils.default_headers()
headers.update({'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36 OPR/38.0.2220.41',
                'X-FORWARDED-FOR': '163.172.175.210',
                'Accept-Language' : 'en'})

TAG_RE = re.compile(r'(?i)<(?!i|\/i).*?>')
TAG_RE_ALL = re.compile(r'<[^>]+>')
TAG_RE_TABLE = re.compile(r'<table(?s).*?table>')

def RemoveTags(text):
    result = TAG_RE_TABLE.sub('', text)    
    return html.unescape(TAG_RE.sub('', result).strip("\n\r\t\v"))

def FindWikiPage(actorName):
    print("Searching in Wikipedia for '" + actorName +"'...")
    startUrl = "https://www.google.com/search?hl=en&q=actor+wiki+"
    actor = actorName.strip().lower().replace(' ', '+')
    page = requests.get(startUrl + actor, headers = headers)
    soup = BeautifulSoup(page.content, "lxml")
    links = soup.select("h3 a")
    
    for link in links:
        href = link["href"]
        if href.startswith("https://en.wikipedia.org"):
            return href
    return ""

def DownloadWikiPage(url):
    print("Downloading...")
    wikiPage = requests.get(url, headers = headers)
    soup = BeautifulSoup(wikiPage.content, "lxml")       
    content = soup.find_all(id = "mw-content-text")
    if content:        
        pTags = content[0].find_all("p")
        articleText = " ".join([str(x) for x in pTags])            
        return RemoveTags(articleText)
    else:
        return ""

def Similar(a, b):
    return SequenceMatcher(None, a, b).ratio()

def ClearTitle(title):
    return title.replace("!", "").replace(" ", "").replace(",", "").replace("'", "").replace(".", "").lower()

def CheckTitle(title):
    for a in title:
        if a.isupper():
            return True
    return False    

def CompareWithDataBase(dbList, title, year, totalCount, exactCount, partlyCount):
    totalCount += 1
    ratio = 0
    tmpRatio = 0
    dbYear = ""
    dbTitle = ""
    for film in dbList:
        tmpRatio = Similar(ClearTitle(title), ClearTitle(film[0]))
        if tmpRatio > ratio or (tmpRatio > ratio - 0.1 and year == film[1]):
            ratio = tmpRatio
            dbYear = film[1]
            dbTitle = film[0]

    if ratio > 0.8:
        if ratio == 1 and year == dbYear:
            exactCount += 1
        else:
            partlyCount += 1
    
    return ratio, totalCount, exactCount, partlyCount

def ProcessWikiPage(text):
    print("Processing...")
    totalCount = 0
    exactCount = 0
    partlyCount = 0
    matches = re.findall(r'(<i>.*?<\/i>)(.{0,10}\d{4})?', text)
    for match in matches:        
        year = match[1]
        if len(year) > 4:
            year = year[-4:]
        title = match[0]
        if title:
            title = TAG_RE_ALL.sub('', title)
            if CheckTitle(title):
                ratio, totalCount, exactCount, partlyCount = CompareWithDataBase(actorsDataBase[actorsIndex[choice]], title, year, totalCount, exactCount, partlyCount)
                
    PrintResult(totalCount, exactCount, partlyCount)

def PrintResult(totalCount, exactCount, partlyCount):
    precision = (exactCount) / len(actorsDataBase[actorsIndex[choice]])
    recall = (exactCount) / totalCount
    f = 2 * (precision * recall) / (precision + recall)
    print("""
          Precision: {0}
          Recall: {1}
          F: {2}
          Exact Match: {3}%
          Partly Match: {4}%
          Garbage: {5}%
          """.format(str(precision),
                     str(recall),
                     str(f),
                     str(round(exactCount / totalCount * 100, 2)),
                     str(round(partlyCount / totalCount * 100, 2)),
                     str(round((totalCount - exactCount - partlyCount)/ totalCount * 100, 2))))
        
nlp = spacy.load("en")

trainingDataBase = np.genfromtxt("training.txt", dtype="str", delimiter = "\t", encoding="utf-8")

actorsDataBase = {}

for actor, film, year in trainingDataBase:
    if actor in actorsDataBase:
        actorsDataBase[actor].append([film, year])
    else:
        actorsDataBase[actor] = [[film, year]]

actorsIndex = {}

i = 0
indexStr = ""

for actor in actorsDataBase:
    i += 1
    actorsIndex[i] = actor
    if indexStr:
        indexStr += "\n"
    indexStr += "{}. {}".format(str(i), actor)

print(indexStr + "\n\n0 - Exit\n")

while True:    
    choice = input("Select actor: ")

    try:
        choice = int(choice)
        
        if choice == 0:
            break

        wikiPageContent = DownloadWikiPage(FindWikiPage(actorsIndex[choice]))
        ProcessWikiPage(wikiPageContent)
    except Exception as e:
        print("Incorrect choice!!!")

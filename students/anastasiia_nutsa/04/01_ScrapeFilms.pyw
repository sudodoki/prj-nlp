import requests
from bs4 import BeautifulSoup

startUrl = "http://www.imdb.com"
subUrl = "/list/ls050274118/"
headers = requests.utils.default_headers()
headers.update({'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36 OPR/38.0.2220.41',
                'X-FORWARDED-FOR': '163.172.175.210',
                'Accept-Language' : 'en'})
processed = 0
saveFile = open("films.txt", "w", encoding="utf8")
actorsPage = requests.get(startUrl + subUrl, headers = headers)
soupActors = BeautifulSoup(actorsPage.content, "lxml")
actors = soupActors.select("h3.lister-item-header a")

for i in (0, 95, 65, 15, 3):
    actorName = actors[i].get_text().strip()
    filmsPage = requests.get(startUrl + actors[i]["href"], headers = headers)
    filmsSoup = BeautifulSoup(filmsPage.content, "lxml")
    filmsSection = filmsSoup.select("div.filmo-category-section")
    films = filmsSection[0].select(".filmo-row")
    
    for film in films:                   
        year = film.select("span.year_column")
        if year:
            year = year[0].get_text().strip().replace("&nbsp;", "")
            if len(year) > 4:
                year = year[0:4]
        else:
            year = ""
        title = film.select("b a")
        if title:
            title = title[0].get_text().strip()
        else:
            title = ""
            
        if year and title:
            saveFile.write('{}\t{}\t{}\n'.format(actorName, title, year))                
            
    processed += 1
    print("Processed: " + actorName)

saveFile.close()

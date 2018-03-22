import requests
from bs4 import BeautifulSoup
import re

TAG_RE = re.compile(r'<[^>]+>')

def remove_tags(text):
    return TAG_RE.sub('', text).replace("р е к л а м а", "").strip("\n\r\t").replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t").replace("\"", "\\\"")

startUrl = "http://forum.lvivport.com/"
subUrl = "forums/istorija-i-vidatni-miscja-lvova.183/"
headers = requests.utils.default_headers()
headers.update({
    'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36 OPR/38.0.2220.41',
})

print("START")

processed = 0

saveFile = open('output.txt', 'w', encoding='utf8')

threadsPage = requests.get(startUrl + subUrl, headers = headers)

while True:
    soupThreads = BeautifulSoup(threadsPage.content, "html.parser")
    threads = soupThreads.find_all("a", class_="PreviewTooltip")

    for thread in threads:
        page = requests.get(startUrl + thread["href"], headers = headers)

        while True:            
            soup = BeautifulSoup(page.content, "lxml")
            content = soup.select("div.messageContent blockquote")
            authors = soup.find_all("a", class_="username author")
            dates = soup.find_all("span", class_="DateTime")
            links = soup.select("div.publicControls a")

            for item in zip(content, authors, dates, links):
                if processed % 100 == 0 and processed > 0:
                    print("Processed: " + str(processed))                
                saveFile.write('{{"content":"{}", "author":"{}", "date":"{}", "link":"{}"}}\n'.format(
                               remove_tags(item[0].get_text()),
                               item[1].get_text(), item[2].get_text(), startUrl + item[3]['href']))
                processed += 1

            nextPage = soup.select("link[rel=\"next\"]")

            if nextPage:
                page = requests.get(startUrl + nextPage[0]["href"], headers = headers)
            else:
                break
            
    nextThreadsPage = soupThreads.select("link[rel=\"next\"]")

    if nextThreadsPage:
        threadsPage = requests.get(startUrl + nextThreadsPage[0]["href"], headers = headers)
    else:
        break

saveFile.close()
print("Total Processed: " + str(processed))

import numpy as np
import langdetect
import re
import requests
from bs4 import BeautifulSoup

TAG_RE = re.compile(r'<[^>]+>')

def remove_tags(text):
    return TAG_RE.sub('', text)

def stringify(text):
    return text.replace('\n', ' ').replace('\r', ' ').replace('\t', ' ')

headers = requests.utils.default_headers()
headers.update({
    'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36 OPR/38.0.2220.41',
})

startUrls = ['https://rozetka.com.ua/ua/mobile-phones/c80003/{}view=list/',
             'https://rozetka.com.ua/ua/tablets/c130309/filter/{}view=list/',
             'https://rozetka.com.ua/ua/notebooks/c80004/filter/{}view=list/',
             'https://rozetka.com.ua/ua/all-tv/c80037/{}view=list/', 
             'https://hard.rozetka.com.ua/ua/motherboards/c80082/{}view=list/',
             'https://hard.rozetka.com.ua/ua/videocards/c80087/{}view=list/',
             'https://hard.rozetka.com.ua/ua/processors/c80083/{}view=list/',
             'https://hard.rozetka.com.ua/ua/ssd/c80109/{}view=list/', 
             'https://rozetka.com.ua/ua/photo/c80001/filter/{}view=list/',
             'https://rozetka.com.ua/ua/video/c80002/{}view=list/',]

processed = 0

def nav_search(teg, soup):
    result = 1
    nav = soup.select(teg)
    try:
        if nav:
            pageNum = nav[0].select('li.paginator-catalog-l-i span')
            if pageNum:
                result = int(pageNum[-1].get_text())
                result += 1
    except Exception as e:
        result = 1
    return result
    
saveFile = open('dataUkr.txt', 'w', encoding='utf8')

for startUrl in startUrls:
    url = startUrl.format('')    
    waresPage = requests.get(url, headers = headers)
    soupWares = BeautifulSoup(waresPage.content, 'lxml')
    maxPg = nav_search('nav.paginator-catalog', soupWares)
    for i in range(1, maxPg):
        if i == 1:
            url = startUrl.format('')
        else:
            url = startUrl.format('page=' + str(i) + ';')
        waresPage = requests.get(url, headers = headers)
        soupWares = BeautifulSoup(waresPage.content, 'lxml')
        links = soupWares.select('div.g-i-list-title a')
        for link in links:
            ware = requests.get(link["href"] + '#tab=comments', headers = headers)
            soup = BeautifulSoup(ware.content, 'lxml')
            maxPg = nav_search('nav.paginator-comments', soup)
            currentPage = 1
            while currentPage <= maxPg:
                reviews = soup.select('article.pp-review-i')
                for review in reviews:
                    processed += 1
                    if processed % 100 == 0:
                        print('processed: ' + str(processed))
                    rate = review.select('meta[itemprop="ratingValue"]')
                    if rate:
                        comment = review.select('div.pp-review-text-i')
                        try:
                            comment = stringify(comment[0].get_text())
                            lang = langdetect.detect(comment)
                            if lang == 'uk':
                                saveFile.write('{}\t{}\n'.format(rate[0]['content'], comment))
                        except langdetect.lang_detect_exception.LangDetectException as e:
                            pass
                currentPage += 1
                ware = requests.get(link["href"] + '/comments/page=' + str(currentPage), headers = headers)
                soup = BeautifulSoup(ware.content, 'lxml')
                
saveFile.close()
        
            
        
    

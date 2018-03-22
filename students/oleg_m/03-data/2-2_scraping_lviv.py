from urllib.request import urlopen
from bs4 import BeautifulSoup

from scrapy import Item, Field
from scrapy.selector import Selector
from scrapy import Spider

lviv_forum_addr = "http://forum.lvivport.com/"

html = urlopen(lviv_forum_addr)
print(html.read())

forums_links = []

html = urlopen(lviv_forum_addr)
bsObj = BeautifulSoup(html, "lxml")
for link in bsObj.findAll("a"):
    if 'href' in link.attrs:
        if link.attrs['href'].startswith('forums/'):
            forums_links.append(link.attrs['href'])
print(forums_links)

threads_links = []
for forum in forums_links:
    html = urlopen(lviv_forum_addr + forum)
    bsObj = BeautifulSoup(html, "lxml")
    for link in bsObj.findAll("a"):
        if 'href' in link.attrs:
            if link.attrs['href'].startswith('threads/'):
                threads_links.append(link.attrs['href'])
        break

print(forums_links)



# class Article(Item):
#     title = Field()
#
#
# class ArticleSpider(Spider):
#     name = "article"
#     allowed_domains = ["en.wikipedia.org"]
#     start_urls = [lviv_forum_addr]
#
#     def parse(self, response):
#         item = Article()
#         title = response.xpath('//h1/text()')[0].extract()
#         print("Title is: "+title)
#         item['title'] = title
#         return item
#
# r = ArticleSpider()
# r.parse(lviv_forum_addr)

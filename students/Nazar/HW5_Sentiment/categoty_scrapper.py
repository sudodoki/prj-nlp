import scrapy
import logging
import pickle
class categoty_scrapper(scrapy.Spider):
    first_n_page=2
    name = 'rozetka.com.ua'
    allowed_domains = ['rozetka.com.ua']
    category_links=[]
    with open ('data_links.txt', 'rb') as fp:
        category_links = pickle.load(fp)
    start_urls=[]
    for link in category_links[:1]:
        for page in range(1,first_n_page):
            print(link)
            start_urls.append('{0}/page={1};sort=popularity'.format(link,page))

    def parse(self, response):
        logging.info(response)
        category = response.xpath("//h1/text()").extract()
        print(category)
        name = response.xpath("//div[@class='g-i-tile-i-box-desc']/*[contains(@class, 'g-i-tile-i-title')]/a/text()").extract()
        comments_link = response.xpath("//div[@class='g-i-tile-i-box-desc']//a[contains(@class, 'g-rating-reviews-link')]/@href").extract()
        logging.info('-----------------------------------------------')
        logging.info('next_link: {0}'.format(name))
        curent_page_goods=list(zip(name, comments_link))
        for name, comments_link in curent_page_goods:
            yield {"name": name, "comments_link": comments_link, 'category':category}
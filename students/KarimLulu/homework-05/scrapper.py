import scrapy
from scrapy.crawler import CrawlerProcess
from scrapy.exceptions import CloseSpider, DropItem
import urllib.parse
import json
from langdetect import detect

MAX_REVIEWS = 100

def process_string(s):
    if isinstance(s, str):
        return s.strip()
    return ""

def process_text(parts, review):
    review["text"] = ""
    review["pros"] = ""
    review["cons"] = ""
    y = "text"
    for part in parts:
        part = process_string(part)
        if part == "Недостатки:":
            y = "cons"
            continue
        elif part == "Достоинства:":
            y = "pros"
            continue
        review[y] = review[y] + part
    return review


class JsonWriterPipeline(object):

    def __init__(self):
        self.data = []
        self.filename = "smartphones.json"

    # def open_spider(self, spider):
    #     self.file = open('items.jl', 'w+')

    def close_spider(self, spider):
        with open(self.filename, "w+") as f:
            json.dump(self.data, f, indent=4)

    def process_item(self, item, spider):
        if not item["reviews"]:
            raise DropItem()
        #line = json.dumps(dict(item), indent=4) + "\n"
        #self.file.write(line)
        self.data.append(dict(item))
        return item


class Product(scrapy.Item):
    title = scrapy.Field()
    price = scrapy.Field()
    attrs = scrapy.Field()
    reviews = scrapy.Field()
    path = scrapy.Field()


class RozetkaSpider(scrapy.Spider):
    name = "posts"
    categories = [
                  # "multivarki/c112986/filter/",
                  # "electric_kettles/c80160/filter/",
                  # #"air_conditioners/c80133/"
                  # "microwaves/c80162/filter/",
                  # "refrigerators/c80125/filter/",
                  # "washing_machines/c80124/filter/",
                  # "freezers/c80203/",
                  # "cookers/c80122/filter/",
                  # "dishwashers/c80123/",
                  # "drying_machines/c80222/",
                  #"notebooks/c80004/filter/"
                  "mobile-phones/c80003/"
                  ]
    start_urls = [urllib.parse.urljoin("https://bt.rozetka.com.ua/", category)
                  for category in categories]
    review_count = 0
    custom_settings = {
        'ITEM_PIPELINES': {
            '__main__.JsonWriterPipeline': 1
        }
    }

    def parse(self, response):
        # scrap links to the products
        for product in response.xpath('//*[contains(@id, "image_item")]/a/@href').extract():
            yield response.follow(product, callback=self.parse_product)

        # follow link to the next page
        next_page = response.xpath('//link[@rel="next"]/@href').extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse)

    def parse_product(self, response):
        product = Product()
        title = response.xpath('//h1[@class="detail-title"]/text()').extract_first()
        title = process_string(title)
        product["title"] = title
        product["price"] = int(response.xpath('//*[@itemprop="price"]/@content').extract_first())
        product["path"] = response.xpath('//*[@class="breadcrumbs-title"]/text()').extract()
        request = scrapy.Request(response.urljoin("comments"),
                                 callback=self.parse_review)
        request.meta["item"] = product
        yield request

    def parse_review(self, response):
        product = response.meta["item"]
        if "reviews" not in product:
            product["reviews"] = []
        for r in response.xpath('//article[@class="pp-review-i"]'): #div[@id="comments"]//
            # if self.review_count >= MAX_REVIEWS:
            #     raise CloseSpider()
            review = {}
            text_parts = r.xpath('.//div[@class="pp-review-text"]//text()').extract()
            review = process_text(text_parts, review)
            full_text = " ".join([review.get("text", ""), review.get("pros", ""),
                                  review.get("cons", "")])
            if detect(full_text) != "uk":
                continue
            stars = r.xpath('.//*[@class="sprite g-rating-stars-i"]/@content').extract_first()
            if stars:
                stars = int(stars)
            else:
                continue
            link = r.xpath('.//a[@class="pp-review-i-link"]/@href').extract_first()
            date = r.xpath('.//meta[@itemprop="datePublished"]/@content').extract_first()
            author = r.xpath('.//span[@class="pp-review-author-name"]/text()').extract_first()
            author = process_string(author)
            review["link"] = response.urljoin(link)
            review["stars"] = stars
            review["date"] = date
            review["author"] = author
            product["reviews"].append(review)
            self.review_count += 1

        # follow link to the next page inside the reviews
        next_page = response.xpath('//link[@rel="next"]/@href').extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            request = scrapy.Request(next_page, callback=self.parse_review)
            request.meta["item"] = product
            yield request
        else:
            yield product

if __name__ == "__main__":
    settings = dict()
    settings['USER_AGENT'] = 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)'
    settings['ITEM_PIPELINES'] = {'__main__.JsonWriterPipeline': 1}
    process = CrawlerProcess(settings=settings)
    spider = RozetkaSpider()
    process.crawl(spider)
    process.start()


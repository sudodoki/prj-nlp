# -*- coding: utf-8 -*-
import scrapy
import polyglot
from polyglot.text import Text
import re

class RozetkaSpider(scrapy.Spider):
    name = 'rozetka'
    allowed_domains = ['rozetka.com.ua']

    def start_requests(self):
        urls = ["http://rozetka.com.ua/dacha-sad-ogorod/c2394297/",
        "https://rozetka.com.ua/santekhnika-i-remont/c4628418/",
        "https://rozetka.com.ua/instrumenty-i-avtotovary/c4627858/",
        "https://rozetka.com.ua/tovary-dlya-doma/c2394287/"]
        for url in urls:
            yield scrapy.Request(url=url, callback=self.get_subcategory_links)

    def get_subcategory_links(self, response):
        urls = response.css('a.pab-h3-link::attr(href)').extract()
        if len(urls) > 0:
            for url in urls:
                yield scrapy.Request(url=url, callback=self.get_subcategory_links)
        else:
            review_blocks = response.css('div.g-rating')
            for block in review_blocks:
                count = block.css('a::attr(data-count)').extract_first()
                href = block.css('a::attr(href)').extract_first()
                if (count is not None) and(int(count) > 0):
                    yield scrapy.Request(url=href, callback=self.get_product_reviews)

    def get_product_reviews(self, response):
        review_blocks = response.css('div.pp-review-inner')
        with open('reviews_rozetka_doma.txt', 'a') as f:

            for block in review_blocks:
                stars = block.css('div[itemprop*=reviewRating] meta[itemprop*=ratingValue]::attr(content)').extract_first()
                if (stars is not None) and (int(stars) > 0):
                    review_text = block.css('div.pp-review-text-i::text').extract_first()
                    if len(review_text) > 0:
                        text = Text(review_text)
                        if text.language.code == 'uk':
                            review_text = review_text.replace('\n', ' ').replace('\r', '')
                            review_text = re.sub('<.+?>', '', review_text)
                            text_row = review_text + '/////' + stars + '\n'
                            f.write(text_row.encode('utf-8'))

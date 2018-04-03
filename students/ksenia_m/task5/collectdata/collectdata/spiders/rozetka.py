# -*- coding: utf-8 -*-
import scrapy
import re


class RozetkaSpider(scrapy.Spider):
    name = 'rozetka'
    allowed_domains = ['rozetka.com.ua']
    start_urls = ['https://rozetka.com.ua/sport-i-uvlecheniya/c4627893/']
    counter = 0

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.f = open('comments.txt', 'w')

    def parse_comments(self, response):
        reviews = response.css('div.pp-review-inner')
        for review in reviews:
            star_element = review.css('span.g-rating-stars-i').extract_first()
            if star_element:
                m = re.search('content="([0-5])"', star_element)
                text = review.css('div.pp-review-text-i::text').extract_first().strip()
                self.f.write(m.group(1) + ":::::" + text + "\n")
                self.counter += 1
                print('C=', self.counter)
        pages = response.css('li.paginator-catalog-l-i a::attr(href)').extract()
        for p in pages:
            yield scrapy.Request(p, callback=self.parse_comments)

    def parse_subcategory(self, response):
        item_links = response.css('a.pab-h3-link::attr(href)').extract()
        found = False
        for a in item_links:
            found = True
            yield scrapy.Request(a, callback=self.parse_subcategory)

        if not found:
            item_links = response.css('div.g-i-tile-i-title a::attr(href)').extract()
            for a in item_links:
                yield scrapy.Request(a + "/comments", callback=self.parse_comments)

    def parse(self, response):
        return self.parse_subcategory(response)


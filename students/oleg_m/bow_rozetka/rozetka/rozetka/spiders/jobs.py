# -*- coding: utf-8 -*-
import scrapy


class JobsSpider(scrapy.Spider):
    name = 'jobs'
    allowed_domains = ['rozetka.com.ua/asus_vivobook_max_x541na_go123/p17982198/#tab=comments']
    start_urls = ['https://rozetka.com.ua/asus_vivobook_max_x541na_go123/p17982198/#tab=comments/']

    def parse(self, response):
        ratings = response.xpath('//div[@class="g-rating-b"]').extract_first()
        stars = 0
        print()
        pass

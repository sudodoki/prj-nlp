# -*- coding: utf-8 -*-
import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
import json
import re

style2rate = {
    'width:100%': 5,
    'width:80%': 4,
    'width:60%': 3,
    'width:40%': 2,
    'width:20%': 1
}

class RozetkaSpider(CrawlSpider):
    name = 'rozetka'
    allowed_domains = ['rozetka.com.ua']
    #start_urls = ['https://rozetka.com.ua/ua/krepkie-napitki/c4594292/filter/page={}/'.format(i+1) for i in range(64)]
    #start_urls = ['https://rozetka.com.ua/ua/vino/c4594285/filter/page={}/'.format(i + 1) for i in range(86)]
    #start_urls = ['https://rozetka.com.ua/ua/all-tv/c80037/page={}/'.format(i + 1) for i in range(86)]
    start_urls = ['https://rozetka.com.ua/ua/mobile-phones/c80003/page={}/'.format(i+1) for i in range(30)]

    rules = (
        Rule(LinkExtractor(restrict_css='.g-i-tile-i-title', process_value=lambda x: x + 'comments/'), callback='parse_reviews_page'),
    )

    def parse_reviews_page(self, response):
        pages = response.css('.paginator-catalog-l-i a::text').extract() or ["1"]
        for page in range(int(pages[-1])):
            yield scrapy.Request(url=response.url + 'page={}/'.format(page+1), callback=self.parse_reviews)

    def parse_reviews(self, response):
        for review in response.css('article.pp-review-i'):
            score = None
            score_el  = review.css('.g-rating-stars-i::attr(style)').extract_first()
            if score_el:
                score = style2rate[score_el.strip()]
            parts = {}
            for i, rp in enumerate(review.css('div.pp-review-text-i')):
                vals = [val.strip() for val in rp.css('::text').extract()]
                if vals[0] == '':
                    parts[vals[1]] = vals[2]
                else:
                    parts['_'] = " ".join(vals)
            yield {'review': json.dumps(parts),
                   'id': review.css('::attr(name)').extract_first().strip(),
                   'scores': score}
# -*- coding: utf-8 -*-
import re
import scrapy
from scrapy import Request
from langdetect import detect


class JobsSpider(scrapy.Spider):
    name = 'jobs'
    allowed_domains = ['rozetka.com.ua']
    start_urls = ['https://hard.rozetka.com.ua/ua/speakers/c80100/view=list/',
                  'https://rozetka.com.ua/ua/headphones/c80027/view=list/',
                  'https://hard.rozetka.com.ua/ua/ups/c80108/view=list/',
                  'https://hard.rozetka.com.ua/ua/processors/c80083/view=list/',
                  'https://rozetka.com.ua/ua/routers/c80193/view=list/',
                  'https://rozetka.com.ua/ua/servers/c125754/view=list/']

    # start_urls = ['https://hard.rozetka.com.ua/ua/monitors/c80089/view=list/',
    #               'https://rozetka.com.ua/ua/printers-mfu/c80007/filter/view=list/']
    # start_urls = ['https://hard.rozetka.com.ua/ua/computers/c80095/filter/view=list/']
    # start_urls = ['https://rozetka.com.ua/ua/tablets/c130309/filter/view=list/']
    # start_urls = ['https://rozetka.com.ua/ua/notebooks/c80004/filter/view=list/']

    def parse(self, response):
        comments_pages = response.xpath('//div[@class="g-i-list-rating"]/div/a/@href').extract()
        for page in comments_pages:
            if '#t_comments' not in page:
                print('PAGE:', page)
                yield Request(page, callback=self.parse_comment)

        link = self.get_next_page(response)
        if link:
            yield Request(link, callback=self.parse)

    def parse_comment(self, response):
        comments = response.xpath('//div[@class="pp-review-inner"]')  # .extract_first()
        for c in comments:
            stars = c.xpath('.//span[@class="sprite g-rating-stars-i"]/@content').extract_first()
            if stars:
                texts = c.xpath('.//div[@class="pp-review-text-i"]').extract()
                texts = [re.sub(r'<.*>', '', t) for t in texts]
                texts = [re.sub(r'\s+|\n', ' ', t).strip() for t in texts]
                normal_text = texts[0]
                lang = detect(normal_text)
                print('LANG:', lang)
                if lang == 'uk':
                    pro_text, con_text = '', ''
                    if len(texts) == 3:
                        pro_text, con_text = texts[1], texts[2]
                    elif len(texts) == 2:
                        if u'Переваги:' in texts[1]:
                            pro_text = texts[1]
                        else:
                            con_text = texts[1]
                    yield {'Stars': stars, 'Response': response, 'Normal_text': normal_text, 'Pro': pro_text, 'Con': con_text}
        next_page = self.get_next_page(response)
        print('LINK_PROD:', next_page)
        if next_page:
            yield Request(next_page, callback=self.parse_comment)

    @staticmethod
    def get_next_page(response):
        paginator = response.xpath('//ul[@class="clearfix inline"]/li')  # .extract()
        next_link = None
        # print('ALL_PAGES:', paginator)
        is_next = False
        for page in paginator:
            # print('PAGES:', page.xpath('@class').extract_first())
            if is_next:
                next_link = page.xpath('.//a/@href').extract_first()
                break
            elif 'active' in page.xpath('@class').extract_first():
                is_next = True
        # print('LINK:', next_link)
        return next_link

# -*- coding: utf-8 -*-
import scrapy


class ComputersSpider(scrapy.Spider):
    counter = 1
    name = 'computers'
    allowed_domains = ['forum.lvivport.com']
    start_urls = ['http://forum.lvivport.com/forums/kompjuteri.16/']

    def parse(self, response):
        NEXT_PAGE_SELECTOR = '.items a ::attr(href)'
        for page in response.css(NEXT_PAGE_SELECTOR).extract():
            if page is not None:
                yield scrapy.Request(response.urljoin(page), callback=self.parse)

        SUBJECT_SELECTOR = '.title a.PreviewTooltip ::attr(href)'
        for page in response.css(SUBJECT_SELECTOR).extract():
            if page is not None:
                yield scrapy.Request(response.urljoin(page), callback=self.parseTopic)


    def parseTopic(self, response):
        NEXT_PAGE_SELECTOR = '.PageNav a ::attr(href)'
        for page in response.css(NEXT_PAGE_SELECTOR).extract():
            if page is not None:
                yield scrapy.Request(response.urljoin(page), callback=self.parseTopic)
        
        ARTICLE_SELECTOR = 'article'
        for article in response.css(ARTICLE_SELECTOR).extract():
            text_file = open("article%s.txt" % self.counter, "w")
            text_file.write(article)
            text_file.close()
            self.counter = self.counter + 1
        #print("::::::", response.css('title').extract_first())


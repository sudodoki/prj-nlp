# -*- coding: utf-8 -*-
import scrapy
from bs4 import BeautifulSoup


class ComputersSpider(scrapy.Spider):
    counter = 1
    name = 'computers'
    allowed_domains = ['forum.lvivport.com']
    start_urls = ['http://forum.lvivport.com/forums/kompjuteri.16/']

    def plain_text(self, html):
        if not html:
            return None
        # remove these tags, complete with contents.
        blacklist = ["script", "style"]

        whitelist = ["article", "blockquote"
            "div", "span", "p", "br", "pre",
            "table", "tbody", "thead", "tr", "td", "a",
            "blockquote",
            "ul", "li", "ol",
            "b", "em", "i", "strong", "u", "font"]

        article = BeautifulSoup(html, "html.parser").find("article")
        if not article:
            return None

        # now strip HTML we don't like.
        for tag in article.findAll():
            if tag.name.lower() in blacklist:
                tag.extract()
            elif not tag.name.lower() in whitelist:
                tag.name = "span"
                tag.attrs = []

        return article.get_text().strip()

    def parse(self, response):
        NEXT_PAGE_SELECTOR = '.items a ::attr(href)'
        for page in response.css(NEXT_PAGE_SELECTOR).extract():
            if page is not None:
                yield scrapy.Request(response.urljoin(page), callback=self.parse)

        SUBJECT_SELECTOR = '.title a.PreviewTooltip ::attr(href)'
        for page in response.css(SUBJECT_SELECTOR).extract():
            if page is not None:
                yield scrapy.Request(response.urljoin(page), callback=self.parse_topic)


    def parse_topic(self, response):
        NEXT_PAGE_SELECTOR = '.PageNav a ::attr(href)'
        for page in response.css(NEXT_PAGE_SELECTOR).extract():
            if page is not None:
                yield scrapy.Request(response.urljoin(page), callback=self.parse_topic)
        
        ARTICLE_SELECTOR = 'article'
        for article in response.css(ARTICLE_SELECTOR).extract():
            text = self.plain_text(article)
            if text:
                text_file = open("articles/article%s.txt" % self.counter, "w")
                text_file.write(text)
                text_file.close()
                self.counter = self.counter + 1
            else:
                print("Can't extract from", article)
        #print("::::::", response.css('title').extract_first())


import scrapy
import logging

class ForumSpider(scrapy.Spider):
    name = 'posts'
    base_url = 'http://forum.lvivport.com'
    start_urls = [
        'http://forum.lvivport.com/forums/scho-u-sviti-chuti.22/'
    ]

    def parse(self, response):
        for href in response.css("ol.discussionListItems div.listBlock h3.title a::attr(href)"):
            url = response.urljoin(href.extract())
            yield scrapy.Request(url, callback=self.parse_posts)

        href = response.css("div.PageNav > nav > a.text::attr(href)")
        yield scrapy.Request(response.urljoin(href.extract_first()), callback=self.parse)

    def parse_posts(self, response):
        title = response.css("div.titleBar > h1").extract_first()
        for elem in response.css("ol.messageList > li.message"):
            author = elem.xpath("@data-author").extract_first()
            if author:
                text = elem.css('div.messageContent > article > blockquote').extract_first()
                yield {'title': title,
                       'author': author,
                       'text': text}



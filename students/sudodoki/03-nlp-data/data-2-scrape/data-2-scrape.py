# coding: utf-8

# http://forum.lvivport.com/forums/vidatni-lvivjani.69/
# topic > .discussionListItem .title a#href
# pages > .PageNav a.PageNavNext:not(.hidden)
# content > .messageInfo.primaryContent .messageContent
# author > .messageInfo.primaryContent .messageMeta .username.author
# date > .messageInfo.primaryContent .messageMeta .DateTime
# link > .messageInfo.primaryContent .messageMeta .datePermalink

import scrapy
import re
from scrapy.crawler import CrawlerProcess

class LvivPortScraper(scrapy.Spider):
    name = 'forum.lvivport.com'
    start_urls = ['http://forum.lvivport.com/forums/vidatni-lvivjani.69/']

    def parse(self, response):
        for next_page in response.css('.discussionListItem .title a::attr(href)'):
            yield response.follow(next_page, self.parse_inner_page)
            
    def parse_inner_page(self, response):
        posts = response.css('.messageInfo.primaryContent')
        posts.extract()
        for post in posts:
            yield {
                'text': re.sub("[\n\r\t]{1,}", "\n", "".join(post.css('.messageText::text').extract()).strip("\n\t\r")),
                'author': post.css('.messageMeta .username.author::text').extract()[0],
                'date': post.css('.messageMeta .DateTime::text').extract()[0],
                'link': "http://forum.lvivport.com/" + post.css('.messageMeta .datePermalink::attr(href)').extract()[0]
            }
        next_page = response.css('.PageNav a.PageNavNext:not(.hidden)')[0]
        if next_page:
            yield response.follow(next_page, self.parse_inner_page)


process = CrawlerProcess({
    'USER_AGENT': 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1)',
    'DOWNLOAD_DELAY': '0.25',
    'FEED_FORMAT': 'jl',
    'FEED_URI': 'output.jsonline',
    'FEED_EXPORT_ENCODING': 'utf-8'
})

process.crawl(LvivPortScraper)
process.start()


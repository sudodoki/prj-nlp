import scrapy
import re
from scrapy.crawler import CrawlerProcess

class BlogSpider(scrapy.Spider):
    name = 'blogspider'
    start_urls = ['http://forum.lvivport.com/threads/fraza-dnja-2.105726/']

    def processContent(self, content):
        return re.sub("[\n\r\t]{1,}", "\n", ''.join(content)).strip()

    def parse(self, response):
        for message in response.css('li.message .messageInfo'):
            
            yield {
                'content': self.processContent(message.css('.messageText::text').extract()),
                'author' : message.css('.messageMeta .author::text').extract_first(),
                'permaLink' : self.start_urls[0] + message.css('.messageMeta .datePermalink::attr(href)').extract()[0],
                'dateTime' : message.css('.messageMeta .DateTime::attr(title)').extract(),
                'sentiment' : self.processContent(message.css('.dark_postrating_outputlist ::text').extract())}

        for next_page in response.css('span.items > a'):
            yield response.follow(next_page, self.parse)

process = CrawlerProcess({
    'FEED_FROMAT' : 'jl',
    'FEED_URI' : 'result.json',
    'FEED_EXPORT_ENCODING': 'utf-8'
})
process.crawl(BlogSpider)
process.start()

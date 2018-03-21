import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor

class Spider(CrawlSpider):
    name = 'spider'
    start_urls = ['http://forum.lvivport.com/forums/pro-avto.25/']
    rules = (
        Rule(LinkExtractor(allow=('http://forum\.lvivport\.com/threads')), callback='parse_page'),
    )

    def parse_page(self, response):
        for m in response.css('li.message'):
            yield {
                'message_id': m.css('::attr(id)').extract_first(),
                'message': m.css('blockquote.messageText ::text').extract_first(),
                'author': m.css('::attr(data-author)').extract_first(),
                'date': m.css('.privateControls a.datePermalink ').extract_first()
            }


class PostsPipeline(object):
    def process_item(self, item, spider):
        print('++++++++ ' + item)

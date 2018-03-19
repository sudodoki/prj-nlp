import scrapy
import urllib.parse

class ForumSpider(scrapy.Spider):
    name = "posts"
    topic = "obs.41"
    base_url = 'http://forum.lvivport.com/forums/'
    start_urls = [urllib.parse.urljoin(base_url, topic)]

    def parse(self, response):
        # scrap links to the threads
        for thread in response.xpath('//a[@class="PreviewTooltip"]/@href').extract()[2:3]:
            yield response.follow(thread, callback=self.parse_thread)

        # follow links to the next pages
        next_page = response.xpath('//link[@rel="next"]/@href').extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse)

    def parse_thread(self, response):
        posts = response.xpath('//div[@class="messageContent"]')
        for post in posts:
            raw_message = post.xpath('.//blockquote[@class="messageText SelectQuoteContainer ugc baseHtml"]/text()')
            message = "".join(raw_message.extract()).strip("\n\r\t ")

        # follow links to the next pages
        next_page = response.xpath('//link[@rel="next"]/@href').extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse_thread)

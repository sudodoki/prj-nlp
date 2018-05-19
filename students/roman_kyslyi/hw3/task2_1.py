import scrapy
import urllib.parse
from scrapy.crawler import CrawlerProcess


XPATH_THREADS = '//a[@class="PreviewTooltip"]/@href'
XPATH_MESSAGE= '//div[@class="messageContent"]'
XPATH_MESSAGE_CONTENT = './/blockquote[@class="messageText SelectQuoteContainer ugc baseHtml"]/text()'
XPATH_NEXT = '//link[@rel="next"]/@href'
HEADERS = {'User-Agent': "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.78 Safari/537.36"}
name = "posts"
topic = "vsjake-rizne.46"
base_url = 'http://forum.lvivport.com/forums/'

class MySpider(scrapy.Spider):

    def parse(self, response):
        for thread in response.xpath(XPATH_THREADS).extract():
            yield response.follow(thread, callback=self.parse_content)
        self._next_page(response)

    def parse_content(self, response):
        posts = response.xpath(XPATH_MESSAGE)
        for k, post in enumerate(posts):
            raw_message = post.xpath(XPATH_MESSAGE_CONTENT)
            message = "".join(raw_message.extract()).strip("\n\r\t ")
            print(message)
            filename = "post_{}".format(k)
            self._save_pst_to_file(filename, message)
        self._next_page(response)

    def _next_page(self, response):
        next_page = response.xpath(XPATH_NEXT).extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            return scrapy.Request(next_page, callback=self.parse_content)

    def _save_pst_to_file(self,filename, message):
        with open(filename, 'w', encoding="utf-8") as f:
            f.write(message)
        f.close()



process = CrawlerProcess(HEADERS)
MySpider.name = name
MySpider.start_urls = [urllib.parse.urljoin(base_url, topic)]
process.crawl(MySpider)
process.start()
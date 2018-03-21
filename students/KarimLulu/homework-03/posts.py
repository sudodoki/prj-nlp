import scrapy
from scrapy.exceptions import CloseSpider
import urllib.parse

from config import init_dir, posts_dir

init_dir(posts_dir)
MAX_POSTS = 100

class ForumSpider(scrapy.Spider):
    name = "posts"
    topic = "obs.41"
    base_url = 'http://forum.lvivport.com/forums/'
    start_urls = [urllib.parse.urljoin(base_url, topic)]
    post_count = 0

    def parse(self, response):
        # scrap links to the threads
        for thread in response.xpath('//a[@class="PreviewTooltip"]/@href').extract():
            yield response.follow(thread, callback=self.parse_thread)

        # follow link to the next page
        next_page = response.xpath('//link[@rel="next"]/@href').extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse)

    def parse_thread(self, response):
        page = response.url.split("/")[-1].split("-")[-1] or "1"
        thread = response.url.split("/")[-2].split(".")[-1]
        posts = response.xpath('//div[@class="messageContent"]')
        for k, post in enumerate(posts):
            if self.post_count >= MAX_POSTS:
                raise CloseSpider()
            raw_message = post.xpath('.//blockquote[@class="messageText SelectQuoteContainer ugc baseHtml"]/text()')
            message = "".join(raw_message.extract()).strip("\n\r\t ")
            filename = f"thread_{thread}_page_{page}_post_{k+1}"
            with (posts_dir / filename).open("w+") as f:
                f.write(message)
            self.post_count += 1

        # follow link to the next page inside the thread
        next_page = response.xpath('//link[@rel="next"]/@href').extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse_thread)

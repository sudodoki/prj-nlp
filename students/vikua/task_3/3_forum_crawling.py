import scrapy
import re


class ForumSpider(scrapy.Spider):
    name = 'posts'
    start_urls = [
        'http://forum.lvivport.com/forums/scho-u-sviti-chuti.22/'
    ]
    page_num_pattern = '(.*\/)page\-(\d+)'

    def parse(self, response):
        href = response.css("div.PageNav > nav > a::attr(href)")[-2]
        pages = re.search(self.page_num_pattern, href.extract())
        sub_url = pages.group(1)
        num_pages = int(pages.group(2))
        for p in range(1, int(num_pages) + 1):
            next_url = '{}page-{}'.format(sub_url, p)
            next_url = response.urljoin(next_url)
            yield scrapy.Request(next_url, callback=self.parse_pages)

    def parse_pages(self, response):
        for href in response.css("ol.discussionListItems div.listBlock h3.title a::attr(href)"):
            url = response.urljoin(href.extract())
            yield scrapy.Request(url, callback=self.parse_posts)

    def parse_posts(self, response):
        href = response.css("div.PageNav > nav > a::attr(href)")
        if href:
            last = href[-2]
            pages = re.search(self.page_num_pattern, last.extract())
            sub_url = pages.group(1)
            num_pages = int(pages.group(2))
            for p in range(1, int(num_pages) + 1):
                next_url = '{}page-{}'.format(sub_url, p)
                next_url = response.urljoin(next_url)
                yield scrapy.Request(next_url, callback=self.parse_post_pages)

    def parse_post_pages(self, response):
        title = response.css("div.titleBar > h1").extract_first()
        for elem in response.css("ol.messageList > li.message"):
            author = elem.xpath("@data-author").extract_first()
            if author:
                text = elem.css('div.messageContent > article > blockquote').extract_first()
                yield {'title': title,
                       'author': author,
                       'text': text}



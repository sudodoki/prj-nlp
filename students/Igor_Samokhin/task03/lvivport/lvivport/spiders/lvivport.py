import scrapy
import re
import os

from lvivport.items import Thread, Post

class LvivSpider(scrapy.Spider):
    name = "lvivport"
    threads = []
        
    def start_requests(self):
        urls = [
            'http://forum.lvivport.com/forums/velodorizhka.92/',
        ]
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response):
        # parse a page with threads
        threads = response.xpath('//li[contains(@class, "discussionListItem")]')
        for t in threads:
            item = Thread()
            # start with thread id, then author of the thread, and topic itself
            thread_id = t.xpath('@id').extract_first()
            item['thread_id'] = int(re.search(r'[0-9]+', thread_id).group())
            item['author'] = t.xpath('@data-author').extract_first()
            item['topic'] = t.xpath('div/div/h3[@class="title"]/a/text()').extract_first()
            item['thread_date'] = t.xpath(
                './/div[@class="posterDate muted"]/span/a/span/text()').extract_first()
            item['thread_n_posts'] = int(t.xpath('.//dl[@class="major"]/dd/text()').extract_first())
            topic_url = t.xpath('div/div/h3[@class="title"]/a/@href').extract_first()
            topic_url = response.urljoin(topic_url)
            item['topic_url'] = topic_url
            item['posts'] = []
            thread_request = scrapy.Request(topic_url, 
                                            callback=self.parse_thread,
                                            meta={'item': item})
            yield thread_request
            
        next_page = response.xpath('//div[@class="PageNav"]/nav/a[contains(text(),\
                                   "Вперед")]/@href').extract_first()
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse)
        
        
    def parse_thread(self, response):
        # parse separate thread
        item = response.meta['item']
        posts = response.xpath('//li[contains(@id, "post")]')
        for post in posts:
            post_item = Post()
            post_id = post.xpath('@id').extract_first()
            post_item['post_id'] = int(re.search(r'[0-9]+', post_id).group())
            post_item['post_author'] = post.xpath('@data-author').extract_first()
            post_item['post_date'] = post.xpath('.//span[@class="DateTime"]/text()').extract_first()
            post_lines = post.xpath('div[contains(@class,\
            "primaryContent")]/div/article/blockquote/text()').extract()
            post_item['post_text'] = '\n'.join([l.strip() for l in post_lines])
            item['posts'].append(post_item)
        
        next_page = response.xpath('//div[@class="PageNav"]/nav/a[contains(text(),\
                                   "Вперед")]/@href').extract_first()
        
        if next_page is not None:
            next_page = response.urljoin(next_page)
            yield scrapy.Request(next_page, callback=self.parse_thread, meta={'item': item})
        else:
            # Only if there are no next pages we know that our item
            # (dictionary of values for thread) is complete.
            # Here we store the results of scrape in separate files
            dirname = 'veloforum/thread_{0}'.format(item['thread_id'])
            if not os.path.exists(dirname):
                os.makedirs(dirname)
            for p in item['posts']:
                filepath = dirname + '/post_{0}'.format(p['post_id'])
                with open(filepath, 'w') as f:
                    first_line = 'post_id: {0}\n'.format(p['post_id'])
                    second_line = 'author: {0}\n'.format(p['post_author'])
                    third_line = 'post_date: {0}\n'.format(p['post_date'])
                    text = first_line + second_line + third_line + p['post_text']
                    f.write(text)
        return item

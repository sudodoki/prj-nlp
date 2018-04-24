import scrapy
import logging
import json
class TopicSpider(scrapy.Spider):
    name = 'forum.lvivport.com'
    allowed_domains = ['forum.lvivport.com']
    TopicList = json.load(open('TopicList.json'))
    start_urls = ['http://forum.lvivport.com/{0}'.format(url['link']) for url in TopicList]

    def parse(self, response):
        logging.info(response)
        topic = response.xpath("//div[@class='titleBar']/h1/text()").extract()
        text = response.xpath("//div[@class='messageContent']").extract()
        user_name = response.xpath("//a[@class='username']/text()").extract()
        date= response.xpath("//a[@class='datePermalink']/span/text()").extract()
        next_link=response.xpath("//a[text()='Вперед >']/@href").extract_first()
        logging.info('-----------------------------------------------')
        logging.info('next_link: {0}'.format(next_link))
        curent_page_posts=list(zip(text, user_name, date))
        for text, user_name, date in curent_page_posts:
            yield {"text": text, "user_name": user_name, 'date':date, 'topic':topic}
        
        yield scrapy.Request('http://forum.lvivport.com/{0}'.format(next_link), callback=self.parse)
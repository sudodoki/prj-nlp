import scrapy
import logging

class TopicListSpider(scrapy.Spider):
    
    def __init__(self):
        self.parsed_data=[]
    name = 'forum.lvivport.com'
    allowed_domains = ['forum.lvivport.com']
    start_urls = [
        'http://forum.lvivport.com/forums/budivnictvo-remont-neruxomist-i-gospodarka.74/',
    ]

    def parse(self, response):
        titles = response.xpath("//a[@class='PreviewTooltip']/text()").extract()
        links = response.xpath("//a[@class='PreviewTooltip']/@href").extract()
        next_link=response.xpath("//a[text()='Вперед >']/@href").extract_first()
        logging.info('-----------------------------------------------')
        logging.info('next_link: {0}'.format(next_link))
        cureent_page_topics=list(zip(titles, links))
        self.parsed_data= self.parsed_data + cureent_page_topics
        for title, link in cureent_page_topics:
            yield {"title": title, "link": link}
        
        yield scrapy.Request('http://forum.lvivport.com/{0}'.format(next_link), callback=self.parse)
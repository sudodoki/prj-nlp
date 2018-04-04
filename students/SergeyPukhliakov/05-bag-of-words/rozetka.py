import scrapy
import re
from scrapy.crawler import CrawlerProcess
from langdetect import detect

class RozetkaSpider(scrapy.Spider):
    name = 'blogspider'
    start_urls = ['https://rozetka.com.ua/ua/instrumenty-i-avtotovary/c4627858/']

    def processContent(self, content):
        return re.sub("[\n\r\t]{1,}", "\n", ''.join(content)).strip()

    def parse_reviews_page(self, response):        
        for comment in response.css('div.pp-review-inner'):
            grade = comment.css('span.g-rating-stars-i::attr(content)').extract_first()
            if grade != None :
                full_text = self.processContent(comment.css('div.pp-review-text-i::text').extract())
                lang = detect(full_text)
                if lang != 'uk': continue
                texts = comment.css('div.pp-review-text-i')
                main_t = texts[0].css('::text').extract_first()
                length = len(texts)
                pos_t = texts[1].css('::text').extract()[2] if length > 1 else ''
                neg_t = texts[2].css('::text').extract()[2] if length > 2 else ''
                yield {
                    'grade':grade,
                    'text' : full_text,
                    'main':main_t,
                    'pos': pos_t,
                    'neg':neg_t
                }
    
    def parse_reviews(self,response):
        for comment in self.parse_reviews_page(response):
            yield comment

        for next_page in response.css('a.paginator-catalog-l-link::attr(href)').extract():
            yield scrapy.Request(next_page, self.parse_reviews_page)

    def parse_page(self, response):
        review_links = []        
        for pr in response.css('a.g-rating-reviews-link'):
            rating = pr.css("span.g-rating-stars-i")
            if rating != []:
                review_links.append(pr)
        
        for link in review_links:
            yield response.follow(link, self.parse_reviews)
    
    def parse_content(self, response):
        for comment in self.parse_page(response):
            yield comment
        for next_page in response.css('a.paginator-catalog-l-link::attr(href)').extract():
            yield scrapy.Request(next_page, self.parse_page)

    def parse(self, response):
        for next_page in response.css('a.sprite-side.pab-items-i-link'):
            yield response.follow(next_page, self.parse_content)
        
process = CrawlerProcess({
    'FEED_FROMAT' : 'jl',
    'FEED_URI' : 'data.json',
    'FEED_EXPORT_ENCODING': 'utf-8'
})
process.crawl(RozetkaSpider)



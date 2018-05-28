# -*- coding: utf-8 -*-
import scrapy
import re
import langid


class RozetkaSpiderSpider(scrapy.Spider):
    name = 'rozetka_spider'
    start_urls = [  'https://bt.rozetka.com.ua/ua/blenders/c80155/filter/',
                    'https://bt.rozetka.com.ua/ua/prochaya-melkaya-tehnika/c123281/',
                    'https://bt.rozetka.com.ua/ua/yaytsevarki/c1948582/',
                    'https://bt.rozetka.com.ua/ua/electric_ovens/c80142/',
                    'https://bt.rozetka.com.ua/ua/breadmakers/c80154/',
                    'https://bt.rozetka.com.ua/ua/deep_fryers/c80150/',
                    'https://bt.rozetka.com.ua/ua/toasters/c80145/',
                    'https://bt.rozetka.com.ua/ua/fruit_vegetable_dryers/c80697/',
                    'https://bt.rozetka.com.ua/ua/steam_cookers/c80157/',
                    'https://bt.rozetka.com.ua/ua/nastolnye-plity/c81667/',
                    'https://bt.rozetka.com.ua/ua/219954/c219954/',
                    'https://bt.rozetka.com.ua/ua/microwaves/c80162/filter/',
                    'https://bt.rozetka.com.ua/ua/yogurt_icecream_machines/c80179/',
                    'https://bt.rozetka.com.ua/ua/grills/c81235/filter/',
                    'https://bt.rozetka.com.ua/ua/sandwich_makers/c80180/',
                    'https://bt.rozetka.com.ua/ua/blinnitsy/c102312/',
                    'https://bt.rozetka.com.ua/ua/airgrills/c81089/',
                    'https://bt.rozetka.com.ua/ua/4625487/c4625487/',
                    'https://bt.rozetka.com.ua/ua/slicers/c80144/',
                    'https://bt.rozetka.com.ua/ua/kitchen_machines/c80163/',
                    'https://bt.rozetka.com.ua/ua/kitchen_scales/c80148/',
                    'https://bt.rozetka.com.ua/ua/penovzbivateli/c2118637/',
                    'https://bt.rozetka.com.ua/ua/mixers/c80156/',
                    'https://bt.rozetka.com.ua/ua/meat_choppers/c80176/filter/',
                    'https://bt.rozetka.com.ua/ua/208243/c208243/',
                    'https://bt.rozetka.com.ua/ua/coffee_machines/c80164/filter/',
                    'https://bt.rozetka.com.ua/ua/coffee_grinders/c80143/',
                    'https://bt.rozetka.com.ua/ua/kulery-dlya-vody/c98002/',
                    'https://bt.rozetka.com.ua/ua/squeezers/c80153/',
                    'https://bt.rozetka.com.ua/ua/electric_kettles/c80160/filter/']

    def __init__(self, category=None, *args, **kwargs):
        super(RozetkaSpiderSpider, self).__init__(*args, **kwargs)
        langid.set_languages(langs = ['en', 'ru', 'uk'])
    

    def parse(self, response):
        rev = response.css('div.g-rating')   
        for r in rev.css('a::attr(href)'):
            yield scrapy.Request(r.extract(), callback=self.parse_review)            
        nav = response.css('#navigation_block')
        for p in nav.css('a::attr(href)'):
            ref = p.extract()
            yield scrapy.Request(ref, callback=self.parse)

    def parse_review(self, response):
        name = response.css('div.detail-title-code.pos-fix.clearfix > h2::text').extract_first().strip()
        comments = response.css('#comments > article')
        for p in comments:
            for c in p.css('div.pp-review-inner'):
                # review
                #rating
                rating = c.css('div.g-rating-b > span > span::attr(content)').extract()
                t = c.css('div.pp-review-text-i')
                text = ''
                pros = ''
                cons = ''
                for tt in t:
                    s = tt.extract()
                    ss = ' '.join([x.strip() for x in tt.xpath('text()').extract()]).strip()
                    if re.search(r'<span class="bold">Переваги:</span>', s):
                        pros = ss
                    elif re.search(r'<span class="bold">Недоліки:</span>', s):
                        cons = ss
                    else:
                        text = ss
                s = ' '.join([text, pros, cons])
                lang = langid.classify(s)
                yield { 'lang':lang[0], 'item': name, 'rating': rating, 'text': text, 'pros': pros, 'cons': cons }


        nav = response.css('#reviews_container_parent > nav')
        for p in nav.css('a::attr(href)'): 
            yield scrapy.Request(p.extract(), callback=self.parse)


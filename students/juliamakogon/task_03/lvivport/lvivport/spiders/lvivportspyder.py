# -*- coding: utf-8 -*-
import scrapy
#from Lvivport.items import LvivportItem


class LvivportspyderSpider(scrapy.Spider):
    name = 'lvivportspyder'
    
    start_urls = ['http://forum.lvivport.com/threads/dtp-u-lvovi.77279/']
    
#    def start_requests(self):
#        urls = [
#            'http://forum.lvivport.com/threads/dtp-u-lvovi.77279/',
#        ]
#        for url in urls:
#            yield scrapy.Request(url=url, callback=self.parse)

    def parse(self, response):
        self.log(response.css('title::text').extract_first())
        
        posts = response.xpath('//li[re:test(@id, "post-\d+")]')
        for p in posts:
            pp = p.css('div.messageInfo.primaryContent > div.messageMeta.ToggleTriggerAnchor > div.privateControls')
            author = pp.css('span > span > a::text').extract()
            date = pp.css('span > a > span::text').extract()

            ppp = p.css('div.messageInfo.primaryContent > div.messageContent > article > blockquote')
            nodes = ppp.xpath('*') # for quotes in the body of the post
            text = ppp.xpath('text()')
            s = ''
            for i in range(len(text)):
                stext = text[i].extract().strip()
                if stext:
                    s = "{}{}\n".format(s, stext)
                if i < len(nodes) and nodes[i].re(r'bbCodeQuote'):
                    q = nodes[i].css('aside > div::text').extract_first().strip()
                    s = "{}{}\n".format(s,q) 
                    tq = [s.strip() for s in nodes[0].css('aside > blockquote > div.quote::text').extract()] 
                    s = "{}{}\n\n".format(s,'\n'.join(tq))
                    
            yield {'author': author,
                   'date': date,
                   'text': s}
                                
        nav = response.xpath('//*[@id="content"]/div/div/div[6]/div[2]/nav')

        for a in nav.css('a::attr(href)'): 
            yield response.follow(a, callback=self.parse)
    


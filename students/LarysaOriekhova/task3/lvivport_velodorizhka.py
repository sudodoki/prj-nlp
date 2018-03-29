# -*- coding: utf-8 -*-
import scrapy


class VelodorizhkaSpider(scrapy.Spider):
    name = 'velodorizhka'
    allowed_domains = ['forum.lvivport.com']

    def start_requests(self):
        urls = ["http://forum.lvivport.com/forums/velodorizhka.92/page-1",
                    "http://forum.lvivport.com/forums/velodorizhka.92/page-2",
                    "http://forum.lvivport.com/forums/velodorizhka.92/page-3"]
        for url in urls:
            yield scrapy.Request(url=url, callback=self.parse)

    ## \brief      Parse page with list of titles. Get articles urls and call parser for each one
    ##
    ##
    def parse(self, response):
        titles_list = response.css('li.discussionListItem')
        for item in titles_list:
            url = item.css("div.listBlock div.titleText h3.title a::attr(href)").extract_first()
            visit_url = url
            yield response.follow(visit_url, self.parse_page)


    ## \brief      Parse main message from article page
    ##
    ##
    def parse_page(self, response):
        messInfo = response.css('div.messageInfo')[0]
        title = response.css('title::text').extract_first()
        title = title.split('|')[0]
        title_to_save = "\n\n<title>" + title + "</title>"

        with open('velodorizhka.txt', 'a') as f:
            main_text = messInfo.css("div.messageContent article blockquote::text").extract()
            main_text = ('. '.join(main_text)).strip()

            main_text = main_text.replace('\t', '')
            main_text = main_text.replace('\n', '')
            text_to_save = "\n<text>" + main_text + "</text>\n"

            f.write(title_to_save.encode('utf-8'))
            f.write(text_to_save.encode('utf-8'))


# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# https://doc.scrapy.org/en/latest/topics/items.html

import scrapy


class Thread(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    thread_id = scrapy.Field()
    author = scrapy.Field()
    thread_date = scrapy.Field()
    thread_n_posts = scrapy.Field()
    topic = scrapy.Field()
    topic_url = scrapy.Field()
    posts = scrapy.Field()

class Post(scrapy.Item):
    post_author = scrapy.Field()
    post_id = scrapy.Field()
    post_date = scrapy.Field()
    post_text = scrapy.Field()
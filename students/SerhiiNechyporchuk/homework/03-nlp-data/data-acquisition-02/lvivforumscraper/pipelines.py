# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: https://doc.scrapy.org/en/latest/topics/item-pipeline.html

import json
import re


class FilePipeline(object):
    def open_spider(self, spider):
        self.file = open('data.jsonl', 'wb')

    def process_item(self, item, spider):
        self.file.write((re.sub(r"\\n|\\t", "", json.dumps(item, ensure_ascii=False)) + "\n").encode('utf8'))

    def close_spider(self, spider):
        self.file.close()

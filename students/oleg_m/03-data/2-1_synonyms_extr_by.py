import os
import re
import xml.sax
import zipfile

# possible names of synonyms in WikiDict (regex like)
SYNONYM_WORDS = ['Сінонімы?']
# regex of the start of synonyms block
REG_SYN_CATEGORY = r'=+\s*(?:{})\s*=+'.format('|'.join(SYNONYM_WORDS))
# regex of the start of any block
REG_CATEGORY = r'=+\s*\w+\s*=+'
# set of values which should be ignored
IGNORE_PAGES = {'Вікіслоўнік:', 'галоўная старонка', 'Шаблон:', 'index', 'Катэгорыя:'}
# regex of the ignored values
IGNORE_REG = r'^(?:{})'.format('|'.join(IGNORE_PAGES))


class WiktionaryHandler(xml.sax.ContentHandler):
    """
    Class that parse wikidictionaty xml file and saves synonyms of the words
    Attributes:
        is_title (bool): if the tag is title (<title> or <\title>
        is_text (bool): if the tag is text (<text> or <\text>
        words (dict): result dict of words synonyms by groups (E.g: word: {1: {syn1, syn2}, 2: {synX, synY}})
        temp_word {str): temp text from the tag
        temp_text {list): temp list from the tag
    """
    def __init__(self):
        super().__init__()
        self.is_title = False
        self.is_text = False
        self.words = dict()
        self.temp_word = ''
        self.temp_text = []

    def startElement(self, name, attrs):
        if name == 'title':
            self.is_title = True
        elif name == 'text':
            self.is_text = True
        else:
            pass

    def characters(self, content):
        if self.is_title and self.is_article(content):
            self.temp_word += content
        if self.is_text:
            self.temp_text.append(content)

    def endElement(self, name):
        if name == 'title':
            self.is_title = False
        elif name == 'text':
            self.words[self.temp_word] = self.get_synonyms()
            self.is_text = False
            self.temp_word = ''
            self.temp_text = []
        else:
            pass

    def get_synonyms(self):
        """
        parse text of the page and get synonyms by groups if exists
        :return: dict of synonyms by group
        """
        values_flag = False
        synonym_values = {}
        group_number = 0
        for line in self.temp_text:
            if re.match(REG_SYN_CATEGORY, line):
                values_flag = True
                group_number += 1
                synonym_values[group_number] = set([])
            elif values_flag:
                if re.match(REG_CATEGORY, line):
                    values_flag = False
                else:
                    synonym_values[group_number] = synonym_values[group_number].union(self.prepare_synonyms(line))
        return {k: v for k, v in synonym_values.items() if v}

    @staticmethod
    def prepare_synonyms(dirty_text):
        """
        Prepare synonyms words from plain text
        :param dirty_text: text from the word page
        :return: set of synonyms
        """
        return {x.strip(' []') for x in re.findall(r'\[[\w\s]+\]', dirty_text) if x != ''}

    @staticmethod
    def is_article(title):
        """
        is the tag the page of word, not additional link
        :param title: text from title tag
        :return: is page or not
        """
        if re.match(IGNORE_REG, title):
            return False
        else:
            return True


# read zip file with WikiDict xml
filename = 'bewiktionary-20180301-pages-articles-multistream.xml'
filepath = 'files/{}'.format(filename)
zf = zipfile.ZipFile('{}.zip'.format(filepath), 'r')
xml_file = zf.extract(filename, 'files/')

# parse xml
parser = xml.sax.make_parser()
parser.setContentHandler(WiktionaryHandler())
parser.parse(open(xml_file, "r"))

# prepare and print synonyms dict
lang_synonyms = {k: v for k, v in parser._cont_handler.words.items() if v}
for k, v in lang_synonyms.items():
    print(k, v)

os.remove(filepath)

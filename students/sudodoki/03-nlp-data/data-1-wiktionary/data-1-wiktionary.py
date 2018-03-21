# coding: utf-8

# wget -O wiktionary.xml.bz2 https://dumps.wikimedia.org/bgwiktionary/20180301/bgwiktionary-20180301-pages-articles-multistream.xml.bz2

from lxml import etree
import bz2
import re

header_re = re.compile(r"===\s*Синоними\s*===", re.M | re.UNICODE)
extra_re = re.compile(r"'''Синоними:?'''", re.M | re.UNICODE)
bullets_re = re.compile(r"^\s*\*", re.M | re.UNICODE)
content_re = re.compile(r"\[\[?([\w\-\s\(\),':]*)\]?\]?", re.M | re.UNICODE)
extra_keys_re = re.compile(r"[А-Я ]+\s*=[\s\n]*", re.M | re.UNICODE)
def cleanup_syns_list(raw_string):
    raw_string = re.sub(header_re, "", raw_string)
    raw_string = re.sub(extra_re, "", raw_string)
    raw_string = re.sub(bullets_re, ", ", raw_string)
    raw_string = re.sub(content_re, r'\1', raw_string)
    raw_string = re.sub(extra_keys_re, "", raw_string)
    return re.sub(r"\s+,\s+", ", ", " ".join(raw_string.split("\n")).strip(", "))

word_re = re.compile(r"ID\s+=\s+(\w+)", re.M | re.UNICODE)
syns_re = re.compile(r"\| (?:СИНОНИМИ|СРОДНИ\s+ДУМИ|ДРУГИ) = ([^\|\}]+)", re.M | re.UNICODE | re.I)
syns_list_re = re.compile(r"===\s*Синоними\s*===\n(\s*\*[^\n]*\n)*", re.M | re.UNICODE | re.I)
def extract_word_and_syns(raw_text):
    result = word_re.search(text)
    word = result and result.group(1)
    syns = None
    if word:
        if "СИНОНИМИ" in text or "СРОДНИ ДУМИ" in text or "ДРУГИ" in text:
            result = syns_re.search(text)
            if (result and result.group(1).strip()):
                syns = cleanup_syns_list(result.group(1).strip())
        if "Синоними" in text:
            result = syns_list_re.search(text)
            if result and result.group(0).strip():
                syns = cleanup_syns_list(result.group(0).strip())
    return (word, syns)

waiting_ns = False
waiting_text = False
text = ''
title = None
all_count = 0
bg_count = 0
syn_count = 0
lang_re = re.compile(r"\{\{-bg-\}\}|ЕЗИК\s*=\s*bg|ЕЗИК\s*=\s*български", re.M | re.UNICODE)
with open('bg_wiktionary_syns.txt', 'w') as output_file:
    for (event, elem) in etree.iterparse(bz2.open("wiktionary.xml.bz2"), events=['start', 'end']):
        if (event == 'start' and elem.tag[-4:] == 'page'):
            waiting_ns = True
            continue
        if (event == 'end' and elem.tag[-4:] == 'page'):
            waiting_ns = False
            waiting_text = False
            title = None
            continue
        if (event == 'end' and waiting_ns and elem.tag[-5:] == 'title'):
            title = elem.text
            continue
        if (waiting_ns and event == 'end' and 'ns' == elem.tag[-2:]):
            if elem.text.strip() == '0':
                waiting_text = True
            continue
        if (waiting_text and event == 'end' and 'text' == elem.tag[-4:]):
            text = elem.text
            if not text:
                continue
            all_count += 1
            if not lang_re.search(text):
                continue
            bg_count += 1
            if bg_count % 1000 == 1:
                print('.', end='')
            (word, syns) = extract_word_and_syns(text)
            if not syns:
                continue
            syn_count += 1
            output_file.write("{} (https://bg.wiktionary.org/wiki/{}): {}\n".format(word, title, syns))
            continue
print('Of {} total articles, {} were in bulgarian and only {} had synonyms'.format(all_count, bg_count, syn_count))
# Of 787273 total articles, 20969 were in bulgarian and only 18253 had synonyms
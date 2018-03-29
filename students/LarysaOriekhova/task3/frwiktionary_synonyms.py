from bz2file import BZ2File
from lxml import etree as et
import re

filename = 'frwiktionary.xml.bz2'

def strip_tag_name(t):
    t = elem.tag
    idx = k = t.rfind("}")
    if idx != -1:
        t = t[idx + 1:]
    return t

def extractseparateSynonyms(synonyms_section, title, word_synonyms):
    syn_text = re.findall('\[\[([^\]]+)\]\]' , synonyms_section)
    for s_word in syn_text:
        if '#' in s_word:
            word = s_word.split('#')[0]
            if (':' in word) or ('|' in word) or ('*' in word):
                continue
            word_synonyms[title].append(word)
        else:
            if (':' in s_word) or ('|' in s_word) or ('*' in s_word):
                continue
            word_synonyms[title].append(s_word)

    syn_text = re.findall('{{[^\|]+\|([^\|^{^}]+)\|[^\|]+}}' , synonyms_section)
    for s_word in syn_text:
        if (':' in s_word) or ('|' in s_word) or ('*' in s_word):
            continue
        word_synonyms[title].append(s_word)

    return word_synonyms

i = 0
with BZ2File(filename) as xml_file:
    parser = et.iterparse(xml_file, events=('end','start'))
    in_page = False
    in_revision = False
    word_title = False
    title = ''
    word_synonyms = {}

    for event, elem in parser:
        tname = strip_tag_name(elem.tag)
        if (event == 'start') and (tname == 'page'):
            in_page = True
        elif (event == 'end') and (tname == 'page'):
            in_page = False
        elif (event == 'start') and (tname == 'revision'):
            in_revision = True
        elif (event == 'end') and (tname == 'revision'):
            in_revision = False
        elif (event == 'start') and (tname == 'title'):
            if (not elem.text) or (len(elem.text) <= 0) or (':' in elem.text):
                word_title = False
            else:
                word_title = True
                title = elem.text
                word_synonyms[title] = []

        elif (event == 'start') and (tname == 'text') and (in_revision == True) \
            and (in_page == True) and (word_title == True):
            page_text = elem.text
            try:
            	m = re.findall('{{S\|synonymes}}[ =]+([^S]+){{S\|' , page_text)
            except:
            	print 'cannot parse page text: ', page_text

            for syn in m:
                word_synonyms = extractseparateSynonyms(syn, title, word_synonyms)

            word_title = False

        i += 1
        # do not create large files
        if i > 1000000:
            break

    for word, syn in word_synonyms.iteritems():
    	if len(syn) > 0:
	        print word, ': ',('; '.join(syn)).encode('utf-8')

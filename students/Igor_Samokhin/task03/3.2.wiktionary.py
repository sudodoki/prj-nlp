from lxml import etree
import bz2
import re
import string

# location of file to use with the script:
# https://dumps.wikimedia.org/svwiktionary/20180301/svwiktionary-20180301-pages-articles-multistream.xml.bz2

# open bz2-commpressed input file
wikidump = bz2.BZ2File('svwiktionary-20180301-pages-articles-multistream.xml.bz2')

# regexp patterns for matching:

# svenpat looks for Swedish part of the text
# synpat looks for synonym part in the Swedish part
# synlistpart extracts words that are actually synonyms

svenpat = r'==Svenska==.*?(\s==[A-Z]|$)'
synpat = r'{{synonymer.*}}'
synlistpat = r'{{synonymer\|(.*)}}'

output_file = open('swedish_synonyms.txt', 'w')

def format_synonyms_string(synlist):
    # helper function to purge recurring unneeded elements from text string
    res = synlist.replace(';', ',')
    res = res.translate(res.maketrans('', '', '[]\''))
    res = re.sub(r'}}{{antonymer.*', '', res)
    res = re.sub(r'#.*', '', res)
    res = re.sub(r'{{.*?}}', '', res)
    res = re.sub(r'<.+>.*?</.+?>', '', res)
    res = re.sub(r'<.+?>', '', res)
    res = re.sub(' +', ' ', res)
    return res

for _, element in etree.iterparse(wikidump):
    # entries of wiktionary are enclosed in <page> tag
    if element.tag == '{http://www.mediawiki.org/xml/export-0.10/}page':
        # now we find title and text (under <revision> element)
        title = element.xpath("*[local-name() = 'title']")[0].text
        if 'Wiktionary:' in title or 'MediaWiki' in title:
            continue
        revision = element.xpath("*[local-name() = 'revision']")[0]
        text = revision.xpath("*[local-name() = 'text']")[0].text
        # wiktionary contains info on the same word in many languages
        # to restrict text to Swedish language, we look for relevant part
        try:
            relevant_part = re.search(svenpat, text, re.S)
        except TypeError:
            continue
        if not relevant_part:
            continue
        # words have multiple meanings with distinct synonyms for each
        # that is why we look for each of several synonym parts of the entry
        synparts = re.findall(synpat, relevant_part.group())
        if not synparts:
            continue
        for i, synpart in enumerate(synparts):
            synlist = re.search(synlistpat, synpart, re.S).group(1)
            # some things must be purged before split
            synlist = format_synonyms_string(synlist)
            # we split in such a way as to preserve commas inside parentheses
            synonyms = re.split(r',\s*(?![^()]*\))', synlist)
            # if there is more than one meaning, we mark meanings in parentheses
            if len(synparts) > 1:
                resstring = '{title} ({n}): {synonyms}'.format(
                title = title, n = i+1, 
                synonyms = ', '.join(synonyms))
            else:
                resstring = '{title}: {synonyms}'.format(
                title = title, synonyms = ', '.join(synonyms))
            output_file.write(resstring + '\n')
        # clear XML element from memory, as we don't need it anymore
        element.clear()

output_file.close()

# as a result of running this file (with a dump file in the home directory), we get swedish_synonyms.txt
import argparse
import bz2
import xml.etree.cElementTree as ET
import itertools as it
import re

NS = '{http://www.mediawiki.org/xml/export-0.10/}'
SYNONYM_LABEL = '=====Synonyymit====='
FINNISH_LABEL = '==Suomi=='
DEFAULT_SENSE = 'OLETUSARVO'


def tag(tag_name):
    return NS + tag_name


def text(elem):
    return "".join(elem.itertext())


def extract_meaning(line):
    # Sorry for messy code
    matches = re.findall(r"""
    \*
    \s?
    (?:
      (?:''\(|\(''|\{\{)
      ([^\[]*)
      (?:\)''|''\)|\}\})
    )?
    \s?
    (.*)
    """, line, re.VERBOSE)
    senses, rest = matches[0]
    sense = senses.split('|')[0].split(',')[0].strip()
    syns = [s.strip().replace('[[', '').replace(']]', '').split('|')[0] for s in rest.split(',')]
    syns = [syn for syn in syns if not re.search(r"\[|''", syn)]
    return sense, syns


def extract_synonyms(txt):
    syns_l = txt.split('\n')
    syns_l = it.dropwhile(lambda l: l != FINNISH_LABEL, syns_l)
    syns_l = list(syns_l)[1:]
    syns_l = it.takewhile(lambda l: not re.match('==\w+==', l), syns_l)
    syns_l = it.dropwhile(lambda l: l != SYNONYM_LABEL, syns_l)
    syns_l = list(syns_l)[1:]
    syns_l = it.takewhile(lambda l: l.startswith('*'), syns_l)
    syns_l = [extract_meaning(l) for l in syns_l]
    return list(syns_l)


def extend_synonyms(dic, word, raw_syns):
    if len(raw_syns) != 0:
        senses = dic.get(word, {})
        for (sense, syns) in raw_syns:
            word_synonyms = senses.get(sense, [])
            word_synonyms += syns
            senses[sense if len(sense) != 0 else DEFAULT_SENSE] = word_synonyms
        dic[word] = senses


if __name__ == "__main__":
    argparser = argparse.ArgumentParser(description='processes warc files and returns pie chart with domains distribtuion')
    argparser.add_argument('--input', required=True, help='Path to finnish dump of wiktionary (.xml.bz2)')
    argparser.add_argument('--output', required=True, help='Path to the output synonyms')
    args = argparser.parse_args()

    all_syns = {}

    for event, elem in ET.iterparse(bz2.open(args.input), events=['end']):
        if event == 'end':
            if elem.tag == tag('page'):
                ns_el = elem.find(tag('ns'))
                if text(ns_el) == '0':
                    title_el = elem.find(tag('title'))
                    title = text(title_el)
                    revision_el = elem.find(tag('revision'))
                    text_el = revision_el.find(tag('text'))
                    txt = text(text_el)
                    syns = extract_synonyms(txt)
                    extend_synonyms(all_syns, title, syns)

    with open(args.output, 'w') as f:
        for (word, senses) in sorted(all_syns.items()):
            for sense, synonyms in sorted(senses.items()):
                f.write('{}\t{}\t{}\n'.format(word, sense, '|'.join(synonyms)))
    print("Saved synonyms to the {}".format(args.output))
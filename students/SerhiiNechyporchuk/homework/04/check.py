import nltk
from nltk.tokenize import sent_tokenize
import pandas as pd
import logging
import spacy
import spacy.symbols as ss
from spacy import displacy
import en_core_web_lg
from stanfordcorenlp import StanfordCoreNLP
from kanren import run, eq, membero, var, conde, Relation, facts
import wikipedia as wiki
import re
import numpy as np
import argparse


nlp = en_core_web_lg.load()


def load_db():
    db = pd.read_csv('books_all.csv', index_col=0)[['authors', 'original_title', 'original_publication_year']]
    db = db.rename(columns={'original_publication_year': 'year'})
    db = db.dropna()
    db['year'] = db['year'].apply(int)
    return db


BOOK_NAME = '__BOOK_NAME__'
REL_DATE = '__REL_DATE__'
variables = {BOOK_NAME, REL_DATE}
REL_SYN = {'release', 'publish', 'accept', 'write'}
BOOK_SYN = {'book', 'novel', 'story', 'piece', 'collection'}
RULES = [
    [((REL_SYN, None, None), (ss.nsubjpass, ss.dobj), (BOOK_NAME, None, ss.PROPN)),
     ((REL_SYN, None, None), (ss.prep,), (['in', 'on'], None, None)),
     ((['in', 'on'], None, None), (ss.pobj,), (REL_DATE, ss.DATE, None))
     ],

    [((REL_SYN, None, None), (ss.nsubjpass, ss.dobj), (BOOK_SYN, None, None)),
     ((BOOK_SYN, None, None), (ss.appos,), (BOOK_NAME, None, ss.PROPN)),
     ((REL_SYN, None, None), (ss.prep,), (['in', 'on'], None, None)),
     ((['in', 'on'], None, None), (ss.pobj,), (REL_DATE, ss.DATE, None))
     ],

    [((REL_SYN, None, None), (ss.nsubjpass, ss.dobj), (BOOK_SYN, None, None)),
     ((BOOK_SYN, None, None), (ss.prep,), (['of'], None, None)),
     ((['of'], None, None), (ss.pobj,), (BOOK_NAME, None, ss.PROPN)),
     ((REL_SYN, None, None), (ss.prep,), (['in', 'on'], None, None)),
     ((['in', 'on'], None, None), (ss.pobj,), (REL_DATE, ss.DATE, None))
     ]
]

AUTHOR = '__AUTHOR__'
AUTHOR_VARS = (AUTHOR,)
WRITER_SYN = {'novelist', 'essayist', 'writer', 'author', 'biographer', 'columnist', 'critic', 'dramatist', 'editor',
              'journalist', 'poet'}

AUTHOR_RULES = [
    [(({'be'}, None, None), (ss.nsubj,), (AUTHOR, ss.PERSON, ss.PROPN)),
     (({'be'}, None, None), (ss.attr,), (WRITER_SYN, None, None))]
]


def is_variable(rchild_lemmas, variables):
    return isinstance(rchild_lemmas, str) and rchild_lemmas in variables


def child_eq(rdeps, rchild, child, variables):
    (rchild_lemmas, rchild_ent, rchild_pos) = rchild
    (child_lemma, child_ent, child_pos, dep) = child.lemma_, child.ent_type, child.pos, child.dep
    if is_variable(rchild_lemmas, variables):
        if (not rchild_pos or rchild_pos == child_pos) and (not rchild_ent or rchild_ent == child_ent) and (
                dep in rdeps):
            return {rchild_lemmas: child}
        else:
            return False
    else:
        return child_lemma in rchild_lemmas and (not rchild_pos or rchild_pos == child_pos) and (
                not rchild_ent or rchild_ent == child_ent) and (dep in rdeps)


def find_entity(doc, idx):
    for ent in doc.ents:
        if idx >= ent.start and idx < ent.end:
            return ent.text


propn_parts = {ss.PROPN, ss.DET, ss.PART}


def find_book_name(doc, tok):
    if tok.dep == ss.appos:
        left = None
        right = None
        for idx in range(tok.i, 0, -1):
            if doc[idx].pos == ss.PUNCT:
                left = idx
                break
        for idx in range(tok.i, len(doc)):
            if doc[idx].pos == ss.PUNCT:
                right = idx
                break

        return doc[left + 1:right].text
    elif tok.pos == ss.PROPN:
        left = tok.i
        right = tok.i + 1
        for idx in range(tok.i, 0, -1):
            if doc[idx].pos in propn_parts:
                left = idx
            else:
                break
        for idx in range(tok.i, len(doc)):
            if doc[idx].pos in propn_parts:
                right = idx
            else:
                break
        return doc[left:right + 1].text


def match(doc, rules, variables):
    for ri, rule in enumerate(rules):
        # print("Rule ", ri)
        matches = []
        bindings = {}
        for subrule in rule:
            ((rhead_lemmas, rhead_ent, _), rdeps, rchild) = subrule
            # print(subrule)
            matched = False
            for tok in doc:
                same_lemma = tok.lemma_ in rhead_lemmas
                same_ent = (not rhead_ent or (rhead_ent == tok.pos))
                same_child = None
                # print("  ", tok)
                for ch in tok.children:
                    # print("    ", rchild, ch)
                    res = child_eq(rdeps, rchild, ch, variables)
                    if res:
                        if isinstance(res, dict):
                            bindings.update(res)
                        same_child = res
                # print("  ", same_lemma, same_ent, same_child)
                matched = same_lemma and same_ent and same_child
                if matched:
                    break
            matches.append(matched)

        # print("MATCHES", matches)
        if all(matches):
            return bindings


def match_book_and_year(doc):
    bindings = match(doc, RULES, variables)
    if bindings:
        book_tok = bindings[BOOK_NAME]
        rel_tok = bindings[REL_DATE]
        return {BOOK_NAME: find_entity(doc, book_tok.i) or find_book_name(doc, book_tok),
                REL_DATE: find_entity(doc, rel_tok.i)}
    else:
        return {}


def match_author(doc, title):
    bindings = match(doc, AUTHOR_RULES, AUTHOR_VARS)
    if bindings:
        author_tok = bindings[AUTHOR]
        return {AUTHOR: title}
    else:
        return {}


def extract_year(rel_date):
    if rel_date:
        m = re.findall(r"\d\d\d\d", rel_date)
        if m:
            return int(m[0])


FIRST_SENTS = 3


def match_all(wikipage):
    collected = []
    author = None
    for i, sent in enumerate(sent_tokenize(wikipage.content)):
        s = nlp(sent)
        binds = match_book_and_year(s)
        date = binds.get(REL_DATE, None)
        year = extract_year(date)
        if i < FIRST_SENTS:
            author = match_author(s, wikipage.title).get(AUTHOR, author)
        elif not author:
            print("No author found, skipping")
            break

        if not year:
            continue
        if binds:
            # print(s)
            # print(binds)
            collected.append([author, year, binds.get(BOOK_NAME)])
    return collected


def ground_truth_year(db, author, title):
    mask = (db['authors'] == author) & (db['original_title'] == title)
    res = db['year'][mask].values
    if len(res) == 0:
        return 0
    else:
        return res[0]


def check_fact(db, fact):
    db_year = ground_truth_year(db, fact[0], fact[2])
    test_year = fact[1]
    return db_year == test_year


def check_facts(db, facts):
    return [check_fact(db, fact) for fact in facts]


def calc_accuracy(db, facts):
    results = check_facts(db, facts)
    return np.sum(results) / len(results)


def evaluate_page(db, page_name):
    wp = wiki.page(page_name)
    facts = match_all(wp)
    return (calc_accuracy(db, facts), len(facts))


if __name__ == "__main__":
    argparser = argparse.ArgumentParser(description='checks wiki page\'s facts' )
    argparser.add_argument('--page', required=True, help='Wiki page title')
    args = argparser.parse_args()

    db = load_db()
    (acc, facts_no) = evaluate_page(db, args.page)
    print("Accuracy: {}".format(acc))
    print("Checked facts: {}".format(facts_no))

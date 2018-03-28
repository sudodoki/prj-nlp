import sys
import os
from SPARQLWrapper import SPARQLWrapper, JSON
from collections import defaultdict
import json
import logging
import spacy
from operator import itemgetter
from itertools import groupby
import re

from config import (data_dir, db_filename, log_fmt, date_fmt,
                    ENDPOINT, QUERY, train_dir, test_dir)
from helpers import  _zip, init_dir
from wiki import get_wiki_json

logging.basicConfig(level=logging.INFO,
                    format=log_fmt,
                    datefmt=date_fmt)
logger = logging.getLogger(__name__)

PIPELINE = {"get_data": {},
            "train_test": {"input": False},
            "apply_rules_to_data": {"input": False}}
DISABLE = ["get_data", "train_test"]
RULES = [("rule_1", 0.1), 
         ("rule_2", 0.3)]
THRESHOLD = 0.5
TRAIN = ["Albrecht Dürer"]
TEST = ["Andrea Mantegna"]
WIKI_MAP = {"Albrecht Dürer": "Albrecht_Dürer",
            "Andrea Mantegna": "Andrea_Mantegna"}

nlp = spacy.load('en')

def get_data():
    logger.info("Fetching the ground truth data")
    sparql = SPARQLWrapper(ENDPOINT)
    sparql.setReturnFormat(JSON)
    sparql.setQuery(QUERY)
    results = sparql.query().convert()
    logger.info(f'Columns: {results["head"]["vars"]}')
    logger.info(f'Fetched {len(results["results"]["bindings"])} records')
    data = defaultdict(list)
    for result in results["results"]["bindings"]:
        artist = result["name"]["value"]
        painting = result["painting"]["value"]
        data[artist].append(painting)
    with (data_dir / db_filename).open("w+") as f:
        json.dump(data, f, indent=4)
    return data

def train_test(labels=None):
    init_dir(train_dir)
    init_dir(test_dir)
    if labels is None:
        with (data_dir / db_filename).open() as f:
            labels = json.load(f)
    data = []
    for key, value in _zip(TRAIN, "train") + _zip(TEST, "test"):
        output = get_wiki_json(WIKI_MAP[key])
        output["y_true"] = labels[key]
        output["type"] = value
        output["title"] = key
        data.append(output)
        with (data_dir / value / f"{key}.json").open("w+") as f:
            json.dump(output, f, indent=4)
    with (data_dir / 'train_test.json').open("w+") as f:
        json.dump(data, f)
    return data

def rule_1(item, doc):
    """Check NER label WORK_OF_ART"""
    work_of_art = filter(lambda x: x.label_=="WORK_OF_ART", doc.ents)
    return [el.text for el in work_of_art]

def get_items(token, doc, output=None):
    if output is None:
        output = []
    lefts = [el.text for el in filter(lambda x: x.dep_ in ["compound", "det"], token.lefts)]
    rights = [el.text for el in filter(lambda x: x.dep_ in ["compound", "det"], token.rights)]
    title = " ".join(lefts + [token.text] + rights)
    output.append(title)
    for child in token.children:
        if child.dep_ == "conj":
            get_items(child, doc, output)
    return output

def rule_2(item, doc):
    v_lemmas = ["produce", "create", "paint", "reproduce"]
    n_lemmas = ["image", "picture", "painting", "work"]
    paintings = []
    for token in doc:
        if token.lemma_ in v_lemmas:
            for child in token.children:
                if child.dep_ == "dobj" and child.lemma_ in n_lemmas:
                    for grand_child in child.children:
                        if grand_child.dep_ == "appos":
                            rez = get_items(grand_child, doc)
                            paintings.extend(rez)
    return list(set(paintings))

def apply_rules_to_item(item):
    paintings = []
    doc = nlp(item["text"])
    for rule, weight in RULES:
        # Strip dates
        output = [(re.sub(r"\s*\(.*\)?", "", el).strip(), weight) 
                 for el in globals()[rule](item, doc)]
        paintings.extend(output)
    for key, gr in groupby(sorted(paintings), key=lambda x: x[0]):
        score = sum([el[-1] for el in gr])
        print(key, score)
    return {"y_true": item["y_true"],
            "type": item["type"],
            "y_pred": set(paintings)}

def apply_rules_to_data(data=None):
    if data is None:
        with (data_dir / 'train_test.json').open() as f:
            data = json.load(f)
    output = []
    for el in data:
        rez = apply_rules_to_item(el)
        output.append(rez)
    return output

def metric():
    pass

def main():
    init_dir(data_dir)
    for pipe, attrs in PIPELINE.items():
        if pipe not in DISABLE:
            if not attrs.pop("input", False):
                output = globals()[pipe](**attrs)
            else:
                output = globals()[pipe](output, **attrs)
    return 0

if __name__ == "__main__":
    code = main()
    sys.exit(code)

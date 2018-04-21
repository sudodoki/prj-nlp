import sys
import os
from SPARQLWrapper import SPARQLWrapper, JSON
from collections import defaultdict, OrderedDict
import json
import logging
import spacy
from operator import itemgetter
from itertools import groupby, product
from difflib import SequenceMatcher
import numpy as np
import re

from config import (data_dir, db_filename, log_fmt, date_fmt,
                    ENDPOINT, QUERY, train_dir, test_dir)
from helpers import  _zip, init_dir, postprocess
from wiki import get_wiki_json

logging.basicConfig(level=logging.INFO,
                    format=log_fmt,
                    datefmt=date_fmt)
logger = logging.getLogger(__name__)

PIPELINE = OrderedDict({"get_db": {},
                       "get_data": {"input": False},
                       "apply_rules_to_data": {"input": False},
                       "get_metrics": {"input": True}})
DISABLE = ["get_db", 
           "get_data", 
           "apply_rules_to_data"
           ]
RULES = [("rule_1", 1),
         ("rule_2", 5),
         ("rule_3", 10),
         ("rule_4", 20)]
TOTAL_WEIGHT = sum([el[-1] for el in RULES])
RULES = [(rule, weight / TOTAL_WEIGHT) for rule, weight in RULES]
SCORE_THRESHOLD = 0
SIMILARITY_THRESHOLD = 0.7
MIN_LEN = 6
TRAIN = ["Albrecht Dürer", "Caravaggio", "El Greco"]
TEST = ["Andrea Mantegna", "Diego Velázquez", "Frans Hals"]
WIKI_MAP = {"Albrecht Dürer": "Albrecht_Dürer",
            "Andrea Mantegna": "Andrea_Mantegna",
            "Caravaggio": "Caravaggio",
            "Diego Velázquez": "Diego_Velázquez",
            "El Greco": "El_Greco",
            "Frans Hals": "Frans_Hals"}

nlp = spacy.load('en')

def get_db():
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
        data[artist].append(painting.lower().strip())
    with (data_dir / db_filename).open("w+") as f:
        json.dump(data, f, indent=4)
    return data

def get_data(labels=None):
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

def rule_1(item, doc, sentences):
    """Check NER label WORK_OF_ART"""
    work_of_art = list(filter(lambda x: x.label_=="WORK_OF_ART", doc.ents))
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

def rule_2(item, doc, sentences):
    v_lemmas = ["produce", "create", "paint", "reproduce"]
    n_lemmas = ["image", "picture", "painting", "work", "engraving", "drawing"]
    paintings = []
    for token in doc:
        if token.lemma_ in v_lemmas:
            for child in token.children:
                if child.dep_ == "dobj" and child.lemma_ in n_lemmas:
                    for grand_child in child.children:
                        if grand_child.dep_ == "appos":
                            rez = get_items(grand_child, doc)
                            paintings.extend(rez)
    return paintings

def rule_3(item, doc, sentences):
    patt = r"[A-Z]\w\. [^,]+|[A-Z]\w+ \w* \w+(?= \(\w+?\)[,])" # Titles with dates inside the parenthesis
    paintings = []
    for match in re.finditer(r"^.*?(engraving|pictur|drawing|painting|work|imag).*?$", 
                             "\n".join(sentences), re.M):
        extraction = re.findall(patt, match.group())
        paintings.extend(filter(lambda x: len(x)>=MIN_LEN, extraction))
    return paintings

def process_subtree(token, doc, output=None):
    if output is None:
        output = []
    parts = []
    for el in token.subtree:
        if el.pos_ in ["PUNCT", "CCONJ"]:
            break
        parts.append(el.text)
    title = " ".join(parts)
    output.append(title)
    for child in token.children:
        if child.dep_ == "conj":
            process_subtree(child, doc, output)
    return output

def rule_4(item, doc, sentences):
    v_lemmas = ["include"]
    paintings = []
    for token in doc:
        if token.lemma_ in v_lemmas:
            for child in token.children:
                if child.dep_ in ["dobj", "pobj"]:
                    rez = process_subtree(child, doc)
                    paintings.extend(rez)
    return paintings

def apply_rules_to_item(item):
    paintings = []
    doc = nlp(item["text"])
    sentences = [re.sub(r"\n|\r", " ", el.string) for el in doc.sents]
    for rule, weight in RULES:
        output = [(postprocess(el), weight)
                 for el in globals()[rule](item, doc, sentences)]
        paintings.extend(set(output))
    y_pred = []
    for key, gr in groupby(sorted(paintings), key=lambda x: x[0]):
        score = sum([el[-1] for el in gr])
        if score >= SCORE_THRESHOLD:
            y_pred.append(key)
    return {"y_true": item["y_true"],
            "type": item["type"],
            "y_pred": y_pred,
            "title": item["title"]}

def apply_rules_to_data(data=None):
    if data is None:
        with (data_dir / 'train_test.json').open() as f:
            data = json.load(f)
    output = []
    for k,el in enumerate(data):
        rez = apply_rules_to_item(el)
        output.append(rez)
        logger.info(f"Processed {k+1} out of {len(data)} items")
    with (data_dir / "extraction.json").open("w+") as f:
        json.dump(output, f, indent=4)
    return output

s = SequenceMatcher(None, "", "")

def calculate_metric(y_true, y_pred):
    d = {}
    for x, y in product(y_true, y_pred):
        s.set_seqs(x, y)
        d[(x, y)] = s.ratio()
    consumed = []
    intersection = 0
    for pair, similarity in sorted(d.items(), key=lambda x: x[-1], reverse=True):
        if pair[0] not in consumed and pair[1] not in consumed and similarity >= SIMILARITY_THRESHOLD:
            intersection += similarity
            consumed.extend(pair)
    return intersection / (len(y_pred) + len(y_true) - intersection)

def get_metrics(data=None):
    if data is None:
        with (data_dir / "extraction.json").open() as f:
            data = json.load(f)
    metrics = defaultdict(list)
    for el in data:
        metric = calculate_metric(el["y_true"], el["y_pred"])
        metrics[el["type"]].append(metric)
    train_score, test_score = np.mean(metrics["train"]), np.mean(metrics["test"])
    logger.info(f"Train: {train_score*100:.3f}%, Test: {test_score*100:.3f}%")
    return train_score, test_score

def main():
    init_dir(data_dir)
    for pipe, attrs in PIPELINE.items():
        if pipe not in DISABLE:
            logger.info(f"Pipeline step `{pipe}`")
            if not attrs.pop("input", False):
                output = globals()[pipe](**attrs)
            else:
                output = globals()[pipe](output, **attrs)
    return 0

if __name__ == "__main__":
    code = main()
    sys.exit(code)

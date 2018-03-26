import sys
import os
from SPARQLWrapper import SPARQLWrapper, JSON
from collections import defaultdict
import json

from config import (data_dir, init_dir, db_filename, log_fmt, date_fmt,
                    ENDPOINT, QUERY)
import logging

logging.basicConfig(level=logging.INFO,
                    format=log_fmt,
                    datefmt=date_fmt)
logger = logging.getLogger(__name__)

PIPELINE = {"get_data": {}}
DISABLE = []#"get_data"]
RULES = ["rule_1"]

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

def train_test():
    pass

def rule_1():
    pass

def apply_rules():
    pass

def metric():
    pass

def main():
    init_dir(data_dir)
    for pipe, attrs in PIPELINE.items():
        if pipe not in DISABLE:
            output = globals()[pipe](**attrs)
    return 0

if __name__ == "__main__":
    code = main()
    sys.exit(code)

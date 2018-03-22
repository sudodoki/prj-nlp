from collections import defaultdict, Counter
from functools import reduce
from pathlib import Path
import sys
import spacy
import json
import logging

logging.basicConfig(level=logging.INFO, 
                    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
                    datefmt="%Y-%m-%d %H:%M:%S")
logger = logging.getLogger(__name__)

# Define verbs
WORDS = ["say", "tell", "speak", "claim", "communicate",
         "narrate", "declare", "respond"]
TOP = 10

# Define files and folders
path = Path(__file__).resolve().parent
data_dir = path.parents[2] / "tasks" / "02-structural-linguistics"
filename = "blog2008.txt"
output_filename = "collocations.txt"

# Load the model
nlp = spacy.load('en_core_web_sm', disable=["ner", "textcat"])

def find_verb(doc, verbs=WORDS, patt="ly"):
    output = defaultdict(list)
    for token in doc:
        if token.lemma_ in verbs and token.pos_ == "VERB":
            adverbs = filter(lambda x: x.pos_=="ADV" and x.text.endswith(patt), token.children)
            output[token.lemma_].extend([el.text.lower() for el in adverbs])
    return output

def merge_dicts(a, b):
    for key, value in b.items():
        a[key].extend(value)
    return a

def main():
    with (data_dir / filename).open() as f:
        data = f.readlines()
        data = [line.strip() for line in data]
    result = (find_verb(doc) for doc in nlp.pipe(data, batch_size=10000, n_threads=-1))
    result = reduce(merge_dicts, result)
    logger.info("Save results to the file")
    with (path / output_filename).open("w+") as f_out:
        for key, value in result.items():
            line = f"{key}: " + ", ".join(str(el) for el in Counter(value).most_common(TOP)) + "\n"
            logger.info(line.strip())
            f_out.write(line)

if __name__=="__main__":
    logger.info("Start processing")
    code = main()
    sys.exit(code)
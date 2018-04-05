from pathlib import Path
import regex as re
import spacy
from spacy.tokenizer import Tokenizer
import logging
import sys
from nltk.corpus import sentiwordnet as swn

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

path = Path(__file__).resolve().parent
data_dir = path.parents[2] / "tasks" / "02-structural-linguistics"
filename = "examiner-headlines.txt"
output_filename = "catchy-headlines.txt"
nlp = spacy.load('en')

MAPPING = {"NOUN": "n", 
           "VERB": "v", 
           "ADJ": "a",
           "ADV": "r"}
EXCLUDE = ["PUNCT", "SYM", "SPACE", "X"]

def prominence(doc):
    return doc.ents

def sentiment(doc, top=5, threshold=0.5):
    mean_pos_sentiment = 0
    for k, token in enumerate(doc):
        if token.pos_ not in EXCLUDE:
            synsets = list(swn.senti_synsets(token.text, pos=MAPPING.get(token.pos_)))[:top]
            token_pos_sentiment = sum(s.pos_score() for s in synsets) / len(synsets) if synsets else 0
            mean_pos_sentiment = (k * mean_pos_sentiment + token_pos_sentiment) / (k + 1)
    return mean_pos_sentiment >= threshold

def superlativeness(doc):
    output = 0
    for token in doc:
        if token.pos_ in ["ADJ", "ADV"]:
            if token.tag_ in ["JJS", "RBS"]:
                output += 1
    return output

def check_catchy(text, top=5, threshold=0.5):
    doc = nlp(text)
    ner = prominence(doc)
    pos_sentiment = sentiment(doc, top=top, threshold=threshold)
    superlat = superlativeness(doc)
    if ner or pos_sentiment or superlat:
        return True
    return False

def main():
    i = 0
    logger.info("Start processing")
    with (data_dir / filename).open() as f_in, (path / output_filename).open("w+") as f_out:
        for k, line in enumerate(f_in):
            is_catchy = check_catchy(line.strip())
            if is_catchy:
                f_out.write(line)
            i += is_catchy
            if (k+1) % 500 == 0:
                logger.info(f"Processed: {k+1}")
    logger.info(f"No. of catchy headlines: {i}")
    return 0

if __name__=="__main__":
    code = main()
    sys.exit(code)

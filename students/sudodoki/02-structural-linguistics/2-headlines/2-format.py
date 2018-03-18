
# coding: utf-8

task_dir = '../../../../tasks/02-structural-linguistics/'

with open(task_dir + 'examiner-headlines.txt', 'r') as f:
    headers = f.readlines()
    headers = [h.strip() for h in headers]


# # I. Formatting

import spacy
import regex as re
from spacy.tokenizer import Tokenizer

infix_re = re.compile(r'''[~]''')
# Overriding splitting on hyphen
def custom_tokenizer(nlp):
    return Tokenizer(nlp.vocab, infix_finditer=infix_re.finditer)

nlp = spacy.load('en_core_web_md')
nlp.tokenizer = custom_tokenizer(nlp)

def format(headline):
    doc = nlp(headline);
    tks = [token for token in doc]
    needs_capitalization = [False for tk in tks]
    # Capitalize nouns, pronouns, adjectives, verbs, adverbs, and subordinate conjunctions.
    to_capitalize = ["PROPN", "NOUN", "PRON", "ADJ", "VERB", "ADV", "SCONJ"]

    for index, token in enumerate(doc):
        if (token.pos_ in to_capitalize):
            needs_capitalization[index] = True
        # Capitalize the first and the last word.
        needs_capitalization[0] = True
        needs_capitalization[-1] = True
    res = ""
    for index, token in enumerate(doc):
        if (needs_capitalization[index]):
            res += token.text.title() + ' ' 
        else:
            res += token.text.lower() + ' ' 
    return res.strip()

fixed_headlines = [format(h) for h in headers]

f = open('format_output.txt','w')
f.write("\n".join(fixed_headlines))
f.close()
print("Output saved to format_output.txt")

total_count = 0
fixed_count = 0
for original, fixed in zip(headers, fixed_headlines):
    total_count += 1
    if not original == fixed:
        fixed_count += 1
print("Fixed %s of total %s (%s %%)" % (fixed_count, total_count, fixed_count / total_count * 100))
# Fixed 4541 of total 5000 (90.82000000000001 %)

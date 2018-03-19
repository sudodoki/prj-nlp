
# coding: utf-8

import spacy
from collections import Counter, defaultdict

nlp = spacy.load('en_core_web_md')

with open(task_dir + "blog2008.txt") as f:
    inputs = f.readlines()

VERBS_OF_INTEREST = set(["add", "advise", "affirm", "allege", "announce", "answer", "articulate", "assert", "aver",
                     "blab", "chat", "claim", "communicate", "confess", "conjecture", "converse", "convey", "declare",
                     "deliver", "demand", "descant", "disclose", "discourse", "divulge", "drawl", "enunciate",
                     "expatiate", "explain", "express", "flap", "gab", "guess", "imply", "inform", "instruct", "lip",
                     "mention", "mumble", "murmur", "mutter", "notify", "opine", "orate", "perorate", "postulate",
                     "proclaim", "pronounce", "rap", "recite", "rehearse", "remark", "repeat", "reply", "report",
                     "respond", "reveal", "rumor", "say", "shout", "speak", "spiel", "state", "suggest", "tell",
                     "utter", "verbalize", "vocalize", "voice", "whisper", "yammer"])

all_counts = defaultdict(Counter)
def add_word(word, adv):
    all_counts[word].update([adv])
def output_counts():
    with open('collocation_output.txt', 'w') as out:

        for verb in all_counts:
            out.write("{}: ".format(verb))
            print("{}: ".format(verb), end='')
            for (adv, count) in all_counts[verb].most_common(10):
                out.write('({}, {})'.format(adv, count))
                print('({}, {})'.format(adv, count), end=' ')
            print('')
            out.write("\n")
i = 0
for input in inputs:
    i += 1
    if (i % 1000 == 0):
        print('{}k'.format(i / 1000))
    doc = nlp(input)
    for token in doc:
        if (token.head.pos_ == 'VERB' and token.head.lemma_ in VERBS_OF_INTEREST):
            if (token.pos_ == 'ADV' and (str(token.lemma_)[-2:] == 'ly')):
                add_word(token.head.lemma_, token.lemma_)

output_counts()


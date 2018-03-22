import spacy
import spacy.symbols as ss
import en_core_web_lg
import pandas as pd
from tqdm import tqdm, tqdm_pandas
from collections import Counter
import argparse

say_syns = set([
    "say", "tell", "speak", "claim", "communicate", "talk",
    "explain", "verbalize", "chat", "communicate", "describe",
    "express", "advertise", "broadcast", "declare", "name"])

def find_collocations(doc, say_syns=say_syns):
    collocations = []
    for tok in doc:
        if tok.lemma_ in say_syns and tok.pos == ss.VERB:
            for verb_child in tok.children:
                if verb_child.pos == ss.ADV and verb_child.text.endswith('ly'):
                    collocations.append((tok.lemma_, verb_child.lemma_))
                    for adv_child in verb_child.children:
                        if adv_child.dep == ss.conj:
                            collocations.append((tok.lemma_, adv_child.lemma_))
    return collocations


if __name__ == '__main__':
    argparser = argparse.ArgumentParser(description='Find 10 the most used adverbs with "say" synonyms')
    argparser.add_argument('--input', required=True, help='Path to the input file')
    args = argparser.parse_args()

    tqdm.pandas()

    nlp = en_core_web_lg.load()
    df = pd.read_table(args.input, names=['text'])[:3000]

    print("Loading data (my take a while)...")
    df = df.assign(doc=df.text.progress_apply(nlp))

    print("\nFinding collocations...")
    df = df.assign(collocations = df['doc'].progress_apply(find_collocations))

    counters = {say_syn: Counter() for say_syn in say_syns}
    for pairs in df['collocations']:
        for (say_syn, adv) in pairs:
            counters[say_syn].update([adv])

    most_common = {say_syn: counter.most_common(10) for say_syn, counter in counters.items()}
    result = pd.DataFrame(
        [(say_syn, ",".join(map(lambda p: p[0], pairs))) for say_syn, pairs in most_common.items()],
        columns=['verb', 'most_used_adv'])

    print('\nVerbs and most used adverbs:')
    print(result)



from collections import Counter
import spacy
nlp = spacy.load('en_core_web_lg')

# synonyms to say, tell, speak, claim, communicate

verb_list = 'pronounce, articulate, enounce, enunciate, order, utter, state, narrate, \
            recite, talk, verbalize, verbalise, address, convey, transmit, assert, \
            maintain, mention, declare, affirm, allege, reply, express, announce, \
            confess, disclose, explain, notify, report, reveal, proclaim, chat, converse, \
            interact, insist, postulate, say, tell, speak, claim, communicate'
verb_list = [v.strip() for v in verb_list.split(', ')]

with open('/../../../tasks/02-structural-linguistics/blog2008.txt') as f:
    blogsents = [s.strip() for s in f]

def find_dep_adverbs(sent, verb_list):
    # sent is a line of text
    doc = nlp(sent)
    res = {}
    verb_tokens = [token for token in doc if token.lemma_ in verb_list]
    for token in verb_tokens:
        res[token.lemma_] = Counter()
        for child in token.children:
            if child.dep_ == 'advmod' and child.lemma_.endswith('ly'):
                res[token.lemma_][child.lemma_] += 1
    return res

def analyze_collocations(sents, verb_list):
    # get the count of verb-dependent adverbs for each verb in verb_list
    # sents is a list of lines from corpus
    counts = {}
    for v in verb_list:
        counts[v] = Counter()
    for s in sents:
        sent_counters = find_dep_adverbs(s, verb_list)
        for v in sent_counters.keys():
            counts[v] += sent_counters[v]
    res = {}
    res_string = ''
    for v in counts.keys():
        res[v] = counts[v].most_common(10)
    return res

def print_collocations(resdic):
    verbs = sorted(resdic.keys())
    res = 'Counts for verbs:\n'
    for v in verbs:
        counts = [f'{adv}: {count}' for adv, count in resdic[v]]
        res += v.upper() + ': ' + ', '.join(counts) + '\n'
    return res

print(print_collocations(analyze_collocations(blogsents, verb_list)))

# in command line: python 3-collocations.py > collocations.txt
# program takes a long time to complete
# prepare and eat a tasty dinner while collocations are counted
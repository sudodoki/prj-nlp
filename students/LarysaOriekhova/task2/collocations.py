import spacy
from spacy.symbols import *
from collections import OrderedDict

nlp = spacy.load('en')
say_synonyms = [ u'confess', u'explain', u'express', u'instruct', u'mention', u'notify', 
                u'order', u'say', u'speak', u'state', u'summon', u'command', u'advertise', 
                u'broadcast', u'connect', u'contact', u'convey', u'correspond', u'disclose', 
                u'disseminate', u'impart', u'inform', u'interact', u'publicize', u'relate', 
                u'reveal', u'suggest', u'tell', u'transfer', u'transmit', u'acquaint', 
                u'advise', u'announce', u'betray', u'break', u'carry', u'declare', 
                u'discover', u'divulge', u'enlighten', u'hint', u'imply', u'network', 
                u'phone', u'proclaim', u'publish', u'raise', u'report', u'signify', u'spread', 
                u'state', u'unfold', u'chat', u'say', u'shout', u'whisper', u'blab', 
                u'chew', u'converse', u'descant', u'discourse', u'drawl', u'enunciate', 
                u'expatiate', u'modulate', u'mouth', u'mumble', u'murmur', u'mutter', 
                u'perorate', u'sound', u'spill', u'vocalize', u'yammer', u'add', u'answer', 
                u'assert', u'claim', u'convey', u'deliver', u'estimate', u'maintain', u'read', 
                u'repeat', u'reply', u'respond', u'voice', u'affirm', u'allege', u'communicate', 
                u'conjecture', u'flap', u'gab', u'guess', u'imagine', u'jaw', u'judge', u'lip', 
                u'opine', u'orate', u'perform', u'pronounce', u'rap', u'recite', u'rehearse', 
                u'relate', u'remark', u'rumor', u'spiel', u'utter', u'verbalize', u'yak']

def find_verb_with_ly(sentence, adverbs_freq):
    
    verbs = list(word for word in sentence if (word.pos == VERB) and (word.lemma_ in say_synonyms))
    for verb in verbs:
        verb_lemma = verb.lemma_
        verb_lemma = verb_lemma.lower()
        adverbs = list(adv for adv in verb.children if adv.pos == ADV)
        for adv in adverbs:
            adv_text = adv.text
            adv_text = adv_text.lower()
            if (len(adv_text) > 2) and (adv_text[len(adv_text) - 2 : len(adv_text)] == 'ly'):
                if not adverbs_freq.has_key(verb_lemma):
                    adverbs_freq[verb_lemma] = {}
                if not adverbs_freq[verb_lemma].has_key(adv_text):
                    adverbs_freq[verb_lemma][adv_text] = 1
                else:
                    adverbs_freq[verb_lemma][adv_text] += 1
    return adverbs_freq
                
adverbs_frequencies = {}
with open('../../../tasks/02-structural-linguistics/blog2008.txt') as blog_corpus:
    lines = blog_corpus.readlines()
    
    for line in lines:
        line = line.decode('utf-8')
        line_doc = nlp(line)
        for sent in line_doc.sents:
            adverbs_frequencies = find_verb_with_ly(sent, adverbs_frequencies)  

for verb, adverbs_fr in adverbs_frequencies.iteritems():
    ordered_freq = OrderedDict(sorted(adverbs_fr.items(), key=lambda t: t[1], reverse=True))
    i = 0
    pitn_str = ''
    for adv, freq in ordered_freq.iteritems():
        if i < 10:
            pitn_str += '(' + adv + ', ' + str(freq) + ') '
            i += 1
    print verb + ': ' + pitn_str
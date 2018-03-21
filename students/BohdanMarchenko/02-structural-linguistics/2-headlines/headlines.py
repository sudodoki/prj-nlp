from collections import defaultdict
import spacy
from nltk.corpus import sentiwordnet as swn
from nltk.corpus.reader.wordnet import WordNetError

to_cap = ["ADJ", "ADV", "NOUN", "PRON", "PROPN", "VERB", "SCONJ"]
to_lower = ["CCONJ", "ADP", "PART", "INTJ"]
all_words = to_cap + to_lower
super_tags = ["JJS", "RBS"]
spacy_to_nltk = {"NOUN":"n", "VERB": "v", "ADJ": "a", "ADV": "r"}
filepath = '../../../../tasks/02-structural-linguistics/examiner-headlines.txt'


class HeadlinesFormatter(object):
    counter = defaultdict(int)
    hyphenized = False
    catchy = []
    nlp = spacy.load("en")

    def capitalize(self, word):
        """chaeck word capitalization"""
        if self.hyphenized:
            self.hyphenized = False
            if word.pos_ in all_words:
                return 'Capitalized by Hyphen'
        elif word.text == "-" and word.whitespace_ != ' ':
            self.hyphenized = True
        elif word.pos_ in to_cap:
            return "Capitalized by POS"
        elif word.pos_ in to_lower:
            return 'Lowered by POS'

    def catch(self, word):
        """catches catchy words"""
        if word.tag_ in super_tags:
            return "{}, {}".format(word.tag_, word)
        elif word.pos_ == "PROPN":
            return  "PROPN, {}".format(word)
        elif word.pos_ in spacy_to_nltk:
            # TODO: convert the following to use spacy
            try:
                if swn.senti_synset('{}.{}.1'.format(word.text, spacy_to_nltk[word.pos_])).obj_score() > 0.5:
                    return "Sentiment word: {}".format(word)
            except WordNetError:
                pass

    def format_headlines(self):
        for line in open(filepath, 'r').readlines():
            sentence = self.nlp(line)
            #capitalize first word
            self.counter["Capitalized by first word"] += 1
            # capitalize last word
            index = -1
            while True:
                #just in order to get word, not symbol
                if sentence[index].pos_ in all_words:
                    self.counter["Capitalized by last word"] += 1
                    break
                index -= 1
            for word in sentence:
                # capitalize
                cap_reason = self.capitalize(word)
                if cap_reason:
                    self.counter[cap_reason] += 1
                # 2. Catch catchy headlines
                catch_reason = self.catch(word)
                if catch_reason:
                    self.catchy.append("{}| {}".format(catch_reason, line))

    def result_out(self):
        #save catchy
        out = open('catchy.txt', 'w')
        for item in set(self.catchy):
            out.write(item)
        #print capitalized
        print("FORMATTING SUMMARY:")
        for k, v in self.counter.items():
            print("{}: {}".format(k, v))
        # FORMATTING SUMMARY:
        # Capitalized by first word: 5000
        # Capitalized by last word: 5000
        # Capitalized by POS: 33609
        # Lowered by POS: 7105
        # Capitalized by Hyphen: 442


if __name__ == "__main__":
    cl = HeadlinesFormatter()
    cl.format_headlines()
    cl.result_out()

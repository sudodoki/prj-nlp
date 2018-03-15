import spacy
from spacy.symbols import *

nlp = spacy.load('en')
capitalize_pos = [NOUN, PROPN, PRON, VERB, ADJ, ADV]


with open('../../../tasks/02-structural-linguistics/examiner-headlines.txt') as test_headlines:
    headlines = test_headlines.readlines()
    
    properly_formatted = 0
    
    for line in headlines:
        line = line.decode('utf-8')
        headline_doc = nlp(line)
        formatted_headline = ''
        
        to_be_capitalized = []
        
        first_word_ind = 0
        last_word_ind = 0
        words_list = list(x for x in headline_doc)
        
        for word in words_list:
            word_text = word.text
            need_capitalization = False
            
            if (word.pos != PUNCT):
                if first_word_ind == 0:
                    first_word_ind = word.i
                if (word_text[0] != '\'') and (word.pos != NUM) and (word.pos != SPACE):
                    last_word_ind = word.i
            
            # Capitalize nouns, pronouns, adjectives, verbs, adverbs
            if word.pos in capitalize_pos:
                need_capitalization = True
            # Capitalize subordinate conjunctions
            if (word.pos == ADP) and (word.dep == mark):
                need_capitalization = True
            
            to_be_capitalized.append(need_capitalization)
        
        # Capitalize the first and the last word
        to_be_capitalized[first_word_ind - 1] = True
        to_be_capitalized[last_word_ind - 1] = True

        # If a word is hyphenated, every part of the word should be capitalized
        hyphenated = list(x for x in words_list if (x.text == '-') and (x.i != 0) and (x.i < len(words_list)))
        for hyphen in hyphenated:
            hyphen_ind = hyphen.i - 1
            if (to_be_capitalized[hyphen_ind + 1] == True) or (to_be_capitalized[hyphen_ind - 1] == True):
                to_be_capitalized[hyphen_ind - 1] = True
                to_be_capitalized[hyphen_ind + 1] = True
        
        headline_changed = False        
        for word in words_list:
            word_text = word.text
            if len(formatted_headline) > 0:
                formatted_headline += ' '
                
            if to_be_capitalized[word.i] == True:
                if (word.pos != NUM) and (word.pos != PUNCT) and (word.pos != SPACE) and (not word_text[0].isupper()):
                    headline_changed = True
                formatted_headline += word_text[0].capitalize() + word_text[1:]
            else:
                if (word.pos != NUM) and (word.pos != PUNCT) and (word.pos != SPACE) and (not word_text[0].islower()):
                    headline_changed = True
                # Lowercase all other parts of speech
                formatted_headline += word_text.lower()
        
        if headline_changed == False:
            properly_formatted += 1
            
        print formatted_headline
        
    print 'properly formated: ', properly_formatted   #result: 401
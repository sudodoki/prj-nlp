from nltk.parse.stanford import StanfordDependencyParser
from nltk.tokenize import sent_tokenize
import nltk
import os
import random
import wikipedia
import time
import re

PATH_TO_JARS = "../../../../jars"
WAIT_BUSY_WIKI_SEC = 30

def find_invent_sentence(person, sentences):
    result = []
    for sent in sentences:
        if "invent" in sent:
            # Header breaks word parsing
            # ==== Micrometer ====
            result.append(re.sub('\s*={2,6}\s*\.+={2,6}\s*','',sent))
    return result;

def ask_wiki_what_invented(person, n_of_try):
    contraptions = []
    if n_of_try <= 0:
        return contraptions
    try:
        page = wikipedia.page(person)
    except wikipedia.exceptions.WikipediaException:
        print("wiki is busy, wait... n of try:", n_of_try)
        time.sleep(WAIT_BUSY_WIKI_SEC)
        return ask_wiki_what_invented(person, n_of_try-1)

    #check invented
    #print(page.content)
    for sent in find_invent_sentence(person, sent_tokenize(page.content)):
            raw = dependency_parser.raw_parse(sent)
            #print("//////")
            #print(sent)
            #print("\\\\\\\\\\")
            for r in raw:
                invented_words = [r.nodes[n] for n in r.nodes if r.nodes[n]['word'] and 'invent' in r.nodes[n]['word']]
                for invented_v in invented_words:
                    print("±±±", sent)
                    #print(invented_v)
                    #print(r)
                    invent_index = -1
                    if 'dobj' in invented_v['deps']:
                        invent_index = invented_v['deps']['dobj'][0]
                    #TODO here is other algorithm for dependecies
                    elif 'ccomp' in invented_v['deps']:
                        invent_index = invented_v['deps']['ccomp'][0]
                    if invent_index == -1:
                        break

                    invention_nodes = r.nodes[invent_index]['deps']
                    invention_words = [invent_index]
                    for dep in invention_nodes:
                        if dep in ['amod', 'compound']:
                            invention_words.append(invention_nodes[dep][0])
                    invention_words.sort()
                    contraption = ""
                    for ind in invention_words:
                        contraption += " " + r.nodes[ind]['word']

                    contraptions.append(contraption)

    return contraptions

def ask_wiki_who_invented(invention):
    return ""

def prepare_data_set(db_name):
    lines = open(db_name).readlines()
    random.shuffle(lines)
    train_len = int(0.8 * len(lines))
    open("train.txt", "w").writelines(lines[0:train_len])
    open("valid.txt", "w").writelines(lines[train_len:])

def contraption_from_wiki(inventor):
    return ask_wiki_what_invented(inventor, 5)


def inventor_from_wiki(contraption):
    return "dududu"

def word_fuzzy_in_list(word, words):
    word_stem = stemmer.stem(word.lower())
    words_stem = [stemmer.stem(w.lower()) for w in words]
    #print("stem:", word_stem, ":of:", word, "in stems:", words_stem, ":of:", words)
    return word_stem in words_stem


def fuzzy_equals(str1, str2):
    if not str1 or not str2:
        return False
    words1 = str1.split(" ")
    words2 = str2.split(" ")
    if len(words1) > len(words2):
        words_max = words1
        words_min = words2
    else:
        words_max = words2
        words_min = words1

    for word in words_min:
        if not word_fuzzy_in_list(word, words_max):
            return False
    return True

def fuzzy_in_list(item, some_list):
    for some_item in some_list:
        if fuzzy_equals(item, some_item):
            return True
    return False

def print_error_rate(test_file_set):
    lines = open(test_file_set)
    right_by_inventor = 0
    right_by_contraption = 0
    total = 0
    for line in lines:
        total += 1
        line = line.replace('\n', '')
        inventor, contraption = line.split(':')
        print("TEST:", line)
        con_from_wiki = contraption_from_wiki(inventor)
        con_found = fuzzy_in_list(contraption, con_from_wiki)
        print("Form wkik contr:", con_from_wiki, ", result:", con_found)
        if con_found:
            right_by_contraption += 1

        if fuzzy_in_list(inventor, inventor_from_wiki(contraption)):
            right_by_inventor += 1

    print("for file %s is %s" % (test_file_set, total))
    print("by contraption:%s" % (right_by_contraption / total))
    print("by inventor:%s" % (right_by_inventor / total))

dependency_parser = StanfordDependencyParser(path_to_jar=os.path.join(PATH_TO_JARS, "stanford-parser-3.9.1.jar"), \
                                             path_to_models_jar=os.path.join(PATH_TO_JARS, "stanford-corenlp-3.9.1-models.jar"))
stemmer = nltk.stem.SnowballStemmer('english')
nltk.download('punkt')

print_error_rate("train_small.txt")
#print_error_rate("train.txt")
#print_error_rate("valid.txt")



#prepare_data_set("train.db")

import spacy
nlp = spacy.load('en_core_web_lg') 
from nltk.corpus import sentiwordnet as swn

# watch for file location
with open('../../../tasks/02-structural-linguistics/examiner-headlines.txt') as f:
    headlines = [h.strip() for h in f]
    
def is_prominent(doc):
    # detect if the headline is prominent
    # we can restrict number of Named Entities to make the program more specific
    # entities from the following list can be deleted for this purpose:
    prom_ents = ['PERSON', 'ORG', 'FACILITY', 'PRODUCT', 'EVENT',
                 'WORK_OF_ART', 'LAW', 'PERCENT', 'MONEY', 'QUANTITY',
                 'ORDINAL', 'CARDINAL']
    for ent in [ent.label_ for ent in doc.ents]:
        if ent in prom_ents:
            return True
    return False

def is_superlative(doc):
    # detect if relative or superlative degree is used in the headline
    supertags = ['JJR', 'JJS', 'RBR', 'RBS']
    return any((token.tag_ in supertags) for token in doc)

def is_charged(doc):
    # detect if the sentiment of the word meanings is high
    tag_dict = {'ADJ': 'a', 'ADV': 'r', 'VERB': 'v', 'NOUN': 'n'}
    words = [(token.lemma_, tag_dict[token.pos_]) for token in doc 
             if token.is_alpha and token.pos_ in tag_dict.keys()]
    sentiments = []
    for word, tag in words:
        sentisents = [synset for synset in swn.senti_synsets(word) 
                     if f'.{tag}.' in synset.unicode_repr()][:5]
        if len(sentisents) == 0:
            continue
        pscore = sum(s.pos_score() for s in sentisents)/len(sentisents)
        nscore = sum(s.neg_score() for s in sentisents)/len(sentisents)
        sentiments.append((pscore, nscore))
    # check if EITHER positive OR negative is above 0.5
    if any((pscore > 0.5 or nscore > 0.5) for pscore, nscore in sentiments):
        return True
    return False

def is_catchy(h):
    # detect if the headline is catchy
    doc = nlp(h)
    return is_prominent(doc) or is_superlative(doc) or is_charged(doc)

def extract_catchy_headlines(headlines, filename):
    # self-explanatory
    with open(filename, 'w') as f:
        for h in headlines:
            if is_catchy(h):
                f.write(h+'\n')

# right now program is not very specific:
# about 70% of headlines are classified as catchy (mostly because of NER)
extract_catchy_headlines(headlines, 'catchy.txt')
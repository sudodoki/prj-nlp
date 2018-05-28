import spacy
from nltk import word_tokenize

def sentence_to_tokens(sentence):
    return word_tokenize(sentence)

# There was an issue with fn below as it resulted in different amount of tokens 
# before/after because of other tokenizing issues
# def toks_to_spacy(nlp, toks):
#     doc = nlp(' '.join(toks))
#     for i in range(len(doc) - 2, -1, -1):
#         if str(doc[i]) == "'" and str(doc[i+1]) == 'm':
#             doc[i:i+2].merge()
#         # there's an issue with whitespace in do n't did n't but that can't be overridden
#         if str(doc[i]) in ["do", "did", "can"] and str(doc[i+1]) == "n't":
#             doc[i:i+2].merge()
#     return doc
# "Solved" it via copying from @ElegantElephant44 (https://git.io/vpOmp), hope it's not violation of CoC
def toks_to_spacy(nlp, tokens):
    doc = spacy.tokens.doc.Doc(nlp.vocab, words=tokens)
    parsed = nlp.parser(doc)
    with_ner = nlp.entity(parsed)
    return with_ner

def sentence_to_sequences(sentence):
    tokens = sentence_to_tokens(sentence)
    labels = [False for tok in tokens]
    return tokens, labels
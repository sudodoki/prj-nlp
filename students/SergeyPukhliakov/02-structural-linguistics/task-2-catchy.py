import spacy
from nltk.corpus import sentiwordnet as swn
from nltk.corpus import wordnet as wn
from statistics import mean

sourceFilePath = "../../../tasks/02-structural-linguistics/examiner-headlines.txt"
resultFilePath = "catchyHeadlines.txt"

nlp = spacy.load('en_core_web_md')

from statistics import mean
def spacyToWn(pos): 
    return {
        'NOUN': wn.NOUN,
        'ADJ': wn.ADJ,
        'ADV' : wn.ADV,
        'VERB' : wn.VERB
    }.get(pos, None)

def isTokenSentiment(token):
    wnPos = spacyToWn(token.pos_)
    if wnPos == None: return False
    sentiments =list(map(lambda s: max(s.pos_score(),s.neg_score()), list(swn.senti_synsets(token.text, wnPos))[0:5]))
    if sentiments == []: return False
    return mean(sentiments) >= 0.5

def isCatchy(headline):      
    doc = nlp(headline)
    if len(doc.ents) > 0 :
        return True
    for token in doc:
        if token.tag_ == 'JJS' or token.tag_ == 'RBS':
            return True
        if isTokenSentiment(token): return True
    return False

def processHeadlines(inputFilePath, outputFilePath):
    with open(inputFilePath, 'r', encoding='utf8') as inputFile:
        headlines = inputFile.readlines()
    catchyHeadlines = list(filter(isCatchy,headlines))
    with open(outputFilePath, 'w', encoding='utf8') as outputFile:
        outputFile.write("".join(catchyHeadlines))

processHeadlines(sourceFilePath, resultFilePath)
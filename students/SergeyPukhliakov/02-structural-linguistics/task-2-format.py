import spacy
import regex as re
from spacy.tokenizer import Tokenizer

sourceFilePath = "../../../tasks/02-structural-linguistics/examiner-headlines.txt"
resultFilePath = "formatedHeadlines.txt"

infix_re = re.compile(r'''[~]''')
nlp = spacy.load('en_core_web_md')

def custom_tokenizer(nlp):
    return Tokenizer(nlp.vocab, infix_finditer=infix_re.finditer)

nlp.tokenizer = custom_tokenizer(nlp)

def hackyTitle(word):
    ind = word.find("'",1)
    if(ind>0):
        return word[:ind].title()+word[ind:]
    else:
        return word.title()
    
def formatToken(token):
    capitalize_pos = ["NOUN", "PROPN", "PRON", "ADJ", "VERB", "ADV", "SCONJ"] 
    if token.pos_ in capitalize_pos:
        return hackyTitle(token.text)
    else:
        return token.text.lower()

def format(headline):      
    doc = nlp(headline)
    lastInd = len(doc)-1
    first = hackyTitle(doc[0].text)
    middle = " ".join(list(map(formatToken, doc[1:lastInd])))
    last = hackyTitle(doc[lastInd].text)
    return "{} {} {}".format(first,middle,last)

def processHeadlines(inputFilePath, outputFilePath):
    with open(inputFilePath, 'r', encoding='utf8') as inputFile:
        with open(outputFilePath, 'w', encoding='utf8') as outputFile:
            headlines = inputFile.readlines()
            matched = 0
            for headLine in headlines:
                original = headLine.strip()
                formated = format(original)
                outputFile.write(formated + '\n')
                if original == formated : matched +=1
    print('Output statistics:{0} from {1} ({2}%)'.format(matched,len(headlines), 100*matched/len(headlines)))

processHeadlines(sourceFilePath, resultFilePath)
#Output statistics:469 from 5000 (9.38%)

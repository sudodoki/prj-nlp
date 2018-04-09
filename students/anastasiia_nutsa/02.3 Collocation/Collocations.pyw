import spacy
from spacy.lemmatizer import Lemmatizer
from spacy.lang.en import LEMMA_INDEX, LEMMA_EXC, LEMMA_RULES
  
nlp = spacy.load('en')
lemmatizer = Lemmatizer(LEMMA_INDEX, LEMMA_EXC, LEMMA_RULES)

def get_adverbs(sentence, verbs):
    result = []
    sentence = nlp(sentence)
    for token in sentence:
        if token.pos_ == "VERB":
            lem = lemmatizer(token.text.lower(), 'VERB')
            if len(lem) > 0:
                if lem[0] in verbs:
                    for child in token.children:
                        if (child.pos_ == "ADV") and (child.text.lower().endswith("ly")):
                            result.append([lem[0], child.text.lower()])
    return result

def add_to_result(result, lst):
    for item in lst:
        if item[0] in result:
            if item[1] in result[item[0]]:
                result[item[0]][item[1]] += 1
            else:
                result[item[0]][item[1]] = 1
        else:
            result[item[0]] = {item[1] : 1}
    return result
                

verbs = ["say", "tell", "speak", "claim", "communicate",
         "state", "allege", "suppose", "talk", "order",
         "pronounce", "assure", "distinguish", "address", "convey",
         "commune", "report", "describe", "declare", "conduct"]

with open('blog2008.txt', 'r', encoding='utf8') as f:
    content = f.readlines()

content = [x.strip() for x in content]

result = {}
processed = 0

for sentence in content:
    if processed % 500 == 0:
        print(str(processed), 'processed')
    result = add_to_result(result, get_adverbs(sentence, verbs))
    processed += 1

print(str(processed), 'processed\n')


for key in result:
    resultStr = key + ":"
    lst = []
    for value in result[key]:
        lst.append([value, result[key][value]])
    lst = sorted(lst, key=lambda x: x[1], reverse=True)
    for i in range(0, min(10, len(lst))):
        item = lst[i]
        if resultStr.endswith(")"):
            resultStr += ","
        resultStr += "({}, {})".format(item[0], str(item[1]))
    print(resultStr)




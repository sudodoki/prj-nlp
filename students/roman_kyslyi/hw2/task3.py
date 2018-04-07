import spacy

synonyms = ['say','about', 'around', 'like', 'estimate', 'replay', 'speak', 'tell','voice',
            'tell','chart', 'chronicle', 'describe', 'narrate', 'recite', 'recount', 'rehearse', 'relate', 'report',
            'articulate', 'enunciate', 'pass', 'state', 'talk', 'utter', 'verbalize', 'vocalize',
            'claim', 'call', 'dibs', 'pretense','communicate','conduct', 'convey', 'give', 'impart', 'spread',
            'transfer', 'transfuse', 'transmit']


def ly(doc, ly):
    adv = []
    for token in doc:
        if token.pos_ == 'VERB' and token.lemma_ in synonyms:
            #adv.add(token.lemma_)
            for dep in token.subtree:
                if ly == 'ly':
                    if dep.pos_ == 'ADV' and not dep.ent_type_ and len(dep.lemma_) > 2 and dep.lemma_[-2:] == 'ly':
                        adv.append(dep.lemma_)
                else:
                    if dep.pos_ == 'ADV':
                        adv.append(dep.lemma_)
    return adv



nlp = spacy.load('en')
input = 'blog2008.txt'
output = 'blog2008_res_ly.txt'

with open(output, 'w', encoding="utf-8") as f2:
    with open(input, 'r', encoding="utf-8") as f:
        for line in f:
            doc4 = nlp(line)
            adv = ly(doc4, 'ly')
            if len(adv)!=0:
                ly_adv = set(adv)
                print(adv)
                f2.write(ly_adv.__repr__().replace('{','').replace('}',', ').replace("'",''))
    f.close()
f2.close()
#print(len(ly_adv))
#print(ly_adv)

from collections import defaultdict
import spacy
verbs = ("add", "announce", "answer", "assert", "claim", "convey", "declare", "deliver", "disclose", "estimate", "express", "maintain", "mention", "read", "repeat", "reply", "report", "respond", "reveal", "speak", "state", "suggest", "tell", "voice", "affirm", "allege", "communicate")
filepath = '../../../../tasks/02-structural-linguistics/blog2008.txt'
import operator

def get_stat():
    nlp = spacy.load("en")
    res = {v: defaultdict(int) for v in verbs}
    counter = 0

    for line in open(filepath, 'r').readlines():
        counter += 1
        sentence = nlp(line)
        for word in sentence:
            if word.text in verbs:
                for child in word.children:
                    if child.pos_ == "ADV" and child.text.endswith('ly'):
                        res[word.text][child.text] +=1
        if counter%1000 == 0:
            print("Line {}".format(counter))

    for k, v in res.items():
        sorted_x = sorted(v.items(), key=operator.itemgetter(1), reverse=True)[:10]
        print("{}: {}".format(k, sorted_x))
# add: [('only', 6), ('really', 5), ('simply', 2), ('quickly', 1), ('Fondly', 1), ('ideologically', 1), ('Normally', 1), ('certainly', 1), ('considerably', 1)]
# announce: [('early', 5), ('publicly', 3), ('openly', 2), ('quickly', 2), ('essentially', 2), ('reportedly', 2), ('quietly', 1), ('electronically', 1), ('Interestingly', 1), ('shortly', 1)]
# answer: [('directly', 5), ('definitively', 3), ('actually', 2), ('really', 2), ('negatively', 1), ('convincingly', 1), ('carefully', 1), ('possibly', 1), ('quickly', 1), ('silkily', 1)]
# assert: [('blithely', 3), ('publicly', 1), ('convincingly', 1), ('simply', 1), ('clearly', 1), ('conclusively', 1)]
# claim: [('falsely', 4), ('credibly', 3), ('brazenly', 1), ('Surely', 1), ('apparently', 1), ('fiercely', 1), ('notably', 1), ('immediately', 1), ('openly', 1), ('remotely', 1)]
# convey: [('fully', 2), ('sharply', 1)]
# declare: [('publicly', 5), ('probably', 1), ('absolutely', 1), ('clearly', 1), ('explicitly', 1), ('immediately', 1), ('suddenly', 1), ('summarily', 1), ('duly', 1), ('entirely', 1)]
# deliver: [('actually', 2), ('really', 2), ('accurately', 1), ('promptly', 1), ('reliably', 1), ('cheerfully', 1), ('Obviously', 1), ('consistently', 1), ('objectively', 1), ('only', 1)]
# disclose: [('publicly', 2), ('fully', 2), ('anonymously', 2), ('frequently', 1), ('directly', 1), ('immediately', 1), ('properly', 1)]
# estimate: [('easily', 1), ('possibly', 1)]
# express: [('actually', 1), ('openly', 1), ('simply', 1), ('personally', 1), ('publicly', 1), ('comprehensively', 1), ('exactly', 1), ('privately', 1), ('possibly', 1)]
# maintain: [('probably', 1), ('Legally', 1), ('merely', 1), ('simultaneously', 1), ('importantly', 1), ('obviously', 1)]
# mention: [('conspicuously', 2), ('especially', 1), ('certainly', 1), ('really', 1), ('thankfully', 1), ('briefly', 1), ('barely', 1), ('respectively', 1), ('explicitly', 1), ('finally', 1)]
# read: [('actually', 16), ('apparently', 8), ('correctly', 7), ('widely', 7), ('simply', 5), ('probably', 5), ('recently', 4), ('carefully', 4), ('regularly', 3), ('avidly', 2)]
# repeat: [('clearly', 1), ('probably', 1), ('undoubtedly', 1), ('simply', 1), ('mindlessly', 1)]
# reply: [('only', 3), ('reasonably', 1)]
# report: [('directly', 4), ('honestly', 2), ('breathlessly', 2), ('fairly', 1), ('only', 1), ('recently', 1), ('publicly', 1), ('professionally', 1), ('accurately', 1), ('properly', 1)]
# respond: [('immediately', 5), ('aggressively', 5), ('quickly', 3), ('personally', 2), ('effectively', 2), ('forcefully', 2), ('directly', 1), ('seriously', 1), ('sharply', 1), ('energetically', 1)]
# reveal: [('officially', 2), ('shockingly', 1), ('blithely', 1), ('actually', 1), ('only', 1), ('transparently', 1), ('quickly', 1), ('suddenly', 1)]
# speak: [('directly', 12), ('publicly', 9), ('only', 5), ('loudly', 4), ('freely', 4), ('plainly', 3), ('probably', 2), ('certainly', 2), ('candidly', 2), ('exactly', 2)]
# state: [('clearly', 6), ('only', 3), ('publicly', 3), ('exactly', 2), ('especially', 1), ('emphatically', 1), ('directly', 1), ('simply', 1), ('formally', 1), ('honestly', 1)]
# suggest: [('strongly', 6), ('publicly', 1), ('regularly', 1), ('certainly', 1), ('gently', 1), ('frequently', 1), ('only', 1), ('Alternatively', 1), ('wishfully', 1), ('alternatively', 1)]
# tell: [('really', 5), ('simply', 5), ('probably', 3), ('honestly', 3), ('likely', 3), ('only', 3), ('exactly', 2), ('possibly', 2), ('usually', 2), ('categorically', 2)]
# voice: [('internationally', 1)]
# affirm: []
# allege: []
# communicate: [('directly', 3), ('effectively', 2), ('freely', 1), ('Really', 1), ('regularly', 1), ('quickly', 1), ('profoundly', 1), ('finally', 1)]

if __name__ == "__main__":
    get_stat()

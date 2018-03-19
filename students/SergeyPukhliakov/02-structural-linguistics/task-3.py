import spacy
nlp = spacy.load('en_core_web_md')

sourceFilePath = "../../../tasks/02-structural-linguistics/blog2008.txt"
resultFilePath = "collocations.txt"

verbs = ["say", "tell", "speak", "claim", "communicate", "verbalize", "vocalize",
         "articulate", "state", "talk", "chart", "report", "utter", "describe", "narrate"]

def analyze(sentence, results):
    doc = nlp(sentence)
    for token in doc:
        if token.pos_ == 'ADV' and token.text.endswith('ly') and token.head.pos_ == 'VERB' and token.head.text in verbs:
            verb = token.head.text
            adv = token.text
            results[verb][adv] = results[verb].get(adv, 0) + 1

def formatResults(results):
    def formatForVerb(advDict):
        return ", ".join(list(map(lambda kv: "({}, {})".format(kv[0],kv[1]), advDict.items())))
    return "\n".join(list(map(lambda kv: "{}:{}".format(kv[0],formatForVerb(kv[1])), results.items())))

def process(inputFilePath, outputFilePath):
    with open(inputFilePath, 'r', encoding='utf8') as inputFile:
        sentences = inputFile.readlines()
    results = {}
    for v in verbs: results[v] = {}
    for s in sentences:
        analyze(s,results)
    with open(outputFilePath, 'w', encoding='utf8') as outputFile:
        outputFile.write(formatResults(results))

process(sourceFilePath, resultFilePath)

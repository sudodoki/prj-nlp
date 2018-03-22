import spacy
import operator

def updateCollocations(collocations, lem, word):
    strWord = str(word).lower()
    goodWord = word.pos_ == 'ADV' and strWord.endswith('ly')
    if (goodWord):
        if (not lem in collocations):
            collocations[lem] = {}
        lemdict  = collocations[lem]
        if (not strWord in lemdict):
            lemdict[strWord] = 1
        else: 
            lemdict[strWord] = lemdict[strWord] + 1
        collocations[lem] = lemdict

    return goodWord


def frequentadverbs(infilePath, outfilePath, limit):
    nlp = spacy.load('en', disable = ['ner', 'textcat'])
    infile = open(infilePath, 'r')
    outfile = open(outfilePath, 'w')

    collocations={}
    synonymslist=["say", "tell", "speak", "claim", "communicate", "announce", "convey", "declare", "deliver", "express", "mention", "respond", "state", "voice", "verbalize", "guess", "suggest", "orate"]

    for line in infile:
        doc = nlp(line)
        for verb in doc:
            lem = verb.lemma_
            if verb.pos_ == 'VERB' and lem in synonymslist:
                for word in verb.children: # only in children!
                    if (updateCollocations(collocations, lem, word)):
                        for w in word.children: # to catch the case "*ly and/but *ly"
                            updateCollocations(collocations, lem, w)

    # sort frequencies and print out
    outline = ""
    for verb in collocations:
       outline = outline + verb + ": "
       sortd = sorted(collocations[verb].items(), key=operator.itemgetter(1), reverse=True)
       count = 0
       for word in sortd:
          outline = outline + str(word)
          count = count + 1
          if (count >= limit or count == len(sortd)):
              outline = outline + "\n"
              break
          else:
              outline = outline + ", "

    print(outline, file=outfile)
    return                       
    
frequentadverbs(infilePath='blog2008.txt', outfilePath='adverbs.txt', limit=10)    
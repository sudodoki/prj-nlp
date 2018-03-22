import spacy
def superlatives(infilePath, outfilePath):
    nlp = spacy.load('en', disable=['parser', 'ner'])
    infile = open(infilePath, 'r')
    outfile = open(outfilePath, 'w')
    superlatives=[]
    for line in infile:
        doc = nlp(line)
        selected=[]
        for tok in doc:
            tag = tok.tag_
            if tag in ["RBS", "AJS"]:   
                selected.append(tok)
        if len(selected)!=0:
            superlatives.append(line)
    print(u''.join(superlatives), file=outfile)
    return


def sentimentlines(infilePath, outfilePath):
    nlp = spacy.load('en', disable=['parser', 'ner'])
    infile = open(infilePath, 'r')
    outfile = open(outfilePath, 'w')
    sentiments = []
    for line in infile:
        doc = nlp(line) 
        highvalue=[]
        for tok in doc:
            posscores=[]
            negscores=[]
            k=1
            tag = tok.tag_
            try:
                if tag in ["NNS", "NN", "NNP", "NNPS"]:
                    t='n'
                elif tag in ["VB", "VBP", "VBG", "VBN", "BVP", "VBZ"]:
                    t='v'
                elif tag in ["JJ", "JJR", "JJS"]:
                    t='a'
                elif tag in ["RB", "RBR", "RBS"]:
                    t='r'
                else: skip
            except BaseException:
                continue
        
            while (k < 6):
                arg=str(tok)+'.'+t+'.0'+str(k)
                k = k+1
                try:
                    result=swn.senti_synset(arg) 
                except BaseException:
                    continue
                else:
                    posscore=result.pos_score()
                    negscore=result.neg_score()
                    posscores.append(posscore)
                    negscores.append(negscore)    
            z=k-1
            average_pos=(sum(posscores))/z
            average_neg=(sum(negscores))/z
        if (average_pos>0.5 or average_neg>0.5):
            highvalue.append(tok)
        if len(highvalue)!=0:
            sentiments.append(line)
    print(u''.join(sentiments), file=outfile)
    return
    

def prominence(infilePath, outfilePath):
    nlp = spacy.load('en', disable=['parser', 'ner'])
    infile = open(infilePath, 'r')
    outfile = open(outfilePath, 'w')
    prominent=[]
    for line in infile:
        doc = nlp(line)
        propernames=[]
        for tok in doc:
            pos = tok.pos_
            if pos in ["PROPN"]:   
                propernames.append(tok)
        if len(propernames)!=0:
            prominent.append(line)
    print(u''.join(prominent), file=outfile)
    return



def catchy(infilePath, outfilePath1, outfilePath2, outfilePath3):
    superlatives(infilePath, outfilePath1)
    sentimentlines(infilePath, outfilePath2)
    prominence(infilePath, outfilePath3)
    return
catchy(infilePath='examiner-headlines.txt', outfilePath1='lines_with_superlatives.txt', outfilePath2='lines_with_sentiments.txt', outfilePath3='lines_with_prominent.txt' )





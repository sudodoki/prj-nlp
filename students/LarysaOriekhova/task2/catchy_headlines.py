import spacy
from spacy.symbols import *


top5_santiments = {}
with open('SentiWordNet.txt') as senti_word_net:
    senti_words = senti_word_net.readlines()

    for line in senti_words:
        if line[0] not in [u'v', u'n', u'r', u'a']:
            continue
        
        fields = line.split('\t')
        pos_tag = fields[0]
        sentiment = float(fields[2]) + float(fields[3]) # may be another logic
        words_str = fields[4]
        
        words = words_str.split(' ')
        
        for w in words:
            parts = w.split('#')
            if (not parts[1].isdigit()) or (int(parts[1]) > 5):
                continue
            word_key = pos_tag + '_' + parts[0]
            if not top5_santiments.has_key(word_key):
                top5_santiments[word_key] = [0, 0]
            top5_santiments[word_key][0] += sentiment
            top5_santiments[word_key][1] += 1
            
print 'top5_santiments built: ', len(top5_santiments)

words_with_sentiment = []
for key, values in top5_santiments.iteritems():
    averege_sent = values[0] / values[1]
    if averege_sent > 0.5:
        words_with_sentiment.append(key)
        
print 'wors_with_sentiment built: ', len(wors_with_sentiment)
        
nlp = spacy.load('en')        
with open('../../../tasks/02-structural-linguistics/examiner-headlines.txt') as test_headlines:
    headlines = test_headlines.readlines()
    
    catchy_headlines = []
    
    for line in headlines:
        
        line = line.decode('utf-8')
        headline_doc = nlp(line)
        
        prominence = False
        for ent in headline_doc.ents:
            if ent.label == PERSON:
                prominence = True
                catchy_headlines.append(line)
                break
        if prominence == True:
            continue
    
        superlativeness = False
        for word in headline_doc:
            if (word.tag_ == 'JJS') or (word.tag_ == 'RBS'):
                superlativeness = True
                catchy_headlines.append(line)
                break
        if superlativeness == True:
            continue
    
        for word in headline_doc:
            pos_tag = ''
            if word.pos == NOUN:
                pos_tag = 'n'
            if word.pos == VERB:
                pos_tag = 'v'
            if word.pos == ADJ:
                pos_tag = 'a'
            if word.pos == ADV:
                pos_tag = 'r'
                
            if len(pos_tag) > 0:
                word_key = pos_tag + '_' + word.text
                # TODO: check phrases, not single words only
                if word_key in words_with_sentiment:
                    catchy_headlines.append(line)
                    break     
                    
for line in catchy_headlines:
    print line
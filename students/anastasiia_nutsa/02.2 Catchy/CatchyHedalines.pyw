import spacy
import nltk
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet as wn
from nltk.corpus import sentiwordnet as swn
from nltk import sent_tokenize, word_tokenize, pos_tag

def check_prominence(text):
    for token in nltk.sent_tokenize(text):
       for chunk in nltk.ne_chunk(nltk.pos_tag(nltk.word_tokenize(token))):
          if hasattr(chunk, 'label'):
              return True
    return False

def check_superlativeness(sentence):
    for token in sentence:
        if token.tag_ in ("JJS", "RBS"):
            return True
    return False

def penn_to_wn(tag):
    #Convert between the PennTreebank tags to simple Wordnet tags
    if tag.startswith('J'):
        return wn.ADJ
    elif tag.startswith('N'):
        return wn.NOUN
    elif tag.startswith('R'):
        return wn.ADV
    elif tag.startswith('V'):
        return wn.VERB
    return None
 
 
def check_sentiment(text):
    raw_sentences = sent_tokenize(text)
    for raw_sentence in raw_sentences:
        tagged_sentence = pos_tag(word_tokenize(raw_sentence)) 
        for word, tag in tagged_sentence:
            score = 0.0
            wn_tag = penn_to_wn(tag)
            if wn_tag not in (wn.NOUN, wn.ADJ, wn.ADV):
                continue
 
            lemma = lemmatizer.lemmatize(word, pos=wn_tag)
            if not lemma:
                continue
 
            synsets = wn.synsets(lemma, pos=wn_tag)
            if not synsets:
                continue
           
            synCount = min(5, len(synsets))

            for i in range(0, synCount):
                synset = synsets[i]
                swn_synset = swn.senti_synset(synset.name())
                #1/2 * first + 1/3 * second + 1/4 * third ... etc
                score += 1 / (i+2) * max(swn_synset.pos_score(), swn_synset.neg_score())

                if score >= 0.5:
                    return True
 
    return False
  
nlp = spacy.load('en')
lemmatizer = WordNetLemmatizer()

processed = -1
catchy = 0

saveFile = open('catchy-headlines.txt', 'w', encoding='utf8')

with open('examiner-headlines.txt', 'r', encoding='utf8') as f:
    content = f.readlines()

content = [x.strip() for x in content]

print('Processing started')

for inputStr in content:
    processed +=1

    if processed % 500 == 0:
        print(str(processed), 'processed')

    if check_prominence(inputStr):    
        saveFile.write(inputStr + '\n')
        catchy += 1
        continue

    sentence = nlp(inputStr)

    if check_superlativeness(sentence):
        saveFile.write(inputStr + '\n')
        catchy += 1
        continue

    if check_sentiment(inputStr):
        saveFile.write(inputStr + '\n')
        catchy += 1
        continue    

saveFile.close()    

print("\nProcessed: {}\nCatchy: {}\n".format(str(processed+1), str(catchy)))

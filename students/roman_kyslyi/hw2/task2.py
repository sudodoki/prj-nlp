import spacy

cap_tags = ['ADJ', 'ADV','NOUN', 'NUM', 'PRON', 'PROPN', 'VERB']
low_tags = ['DET', 'ADP', 'CCONJ', 'PART', 'INTJ']


class Corrector():

    def __init__(self, nlp):
        self.nlp = nlp
        self.all = 0
        self.correct = 0


    def _first_last_words(self, word_list):
        if word_list[0].islower(): word_list[0]=word_list[0].capitalize()
        if word_list[-1].islower(): word_list[-1]=word_list[-1].capitalize()
        return word_list

    def rules(self, line):
        self.all +=1
        doc4 = nlp(line)
        list = []
        for token in doc4:
            #print(token)
            if sum(1 for letter in token.text if letter.isupper()) > 1: tok = token.text
            elif token.text == "n't": tok = token.text
            elif token.pos_ in cap_tags: tok = token.text.capitalize()
            elif token.pos_ in low_tags: tok =token.text.lower()
            else: tok = token.text
            list.append(tok)
        list = self._first_last_words(list)
        line1 = ' '.join(list).replace(" '","'").replace(' ,',',').replace(' .','.').replace(' ;',';')\
            .replace('- ','-').replace(' -','-').replace(' )',')').replace('( ','(').replace(" 's","'s")\
            .replace(' :',':').replace(" n't","n't").replace(" !","!").replace(" ?","?").replace(" #","#") # not the best solution, have to changed to regexp, but due to the lack of time works for now
        if line1.strip() == line.strip(): self.correct +=1
        return line1


nlp = spacy.load('en')
input = 'examiner-headlines.txt'
output = 'examiner-headlines_res.txt'
corrector = Corrector(nlp)
with open(output, 'w', encoding="utf-8") as f2:
    with open(input, 'r', encoding="utf-8") as f:
        for line in f:
            print(line)
            c = corrector.rules(line)
            print(c)
            f2.write(c)
    f.close()
f2.close()
print('All: {}, Correct:{}; '.format(corrector.all,corrector.correct))
# All: 5000, Correct:538;

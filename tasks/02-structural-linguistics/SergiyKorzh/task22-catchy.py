import nltk

#supernative
sup_tags = ["JJR", "JJS", "RBR", "RBS"] 

#prominence
prom_tags = ["NNP", "NNPS"] 

			
def is_supernative(word, tag):
    return (tag in sup_tags)

def is_prominent(word, tag):
    return (tag in prom_tags)


swn_filename = "SentiWordNet_3.0.0_20130122.txt"

sent_words = { }

def load_SentiWordNet():
    with open(swn_filename, mode="r", encoding="utf8") as swn_file:
        for line in swn_file:
            if not line.startswith("#"):                
                parts = line.split("\t")
                score = float(parts[2])
                #find all words in part[4]
                #for each word:
                #word_data = sent_words.get(word, (0,0))
                #word_data[0] += 1
                #word_data[1] += score               


def has_sentiment(word, tag):
    data = sent_words.get(word, (0, 0.0))
    return data[0] > 0 and data[1]/data[0] > 0.5


def is_catchy(word, tag):
    return is_prominent(word, tag) \
        or is_supernative(word, tag) \
        or has_sentiment(word, tag)

tokenizer = nltk.tokenize.TreebankWordTokenizer()

src_filename = "examiner-headlines.txt"

dest_filename = "examiner-headlines-catchy.txt"

line_count = 0
catchy_count = 0

with open(dest_filename, mode="w", encoding="utf8") as  dest_file:
    with open(src_filename, mode="r", encoding="utf8") as src_file:
        for line in src_file:
            line_count += 1
            tokens = tokenizer.tokenize(line)
            tags = nltk.pos_tag(tokens)
            i = 0
            catchy_title = False
            for wtag in tags:    
                word = wtag[0]
                if is_catchy(word, wtag[1]):
                    catchy_title = True
                i += 1
            
            if catchy_title:
                catchy_count += 1
                dest_file.write(line)

print("{} headlines processed, {} are catchy".format(line_count, catchy_count))
#5000 headlines processed, 4351 are catchy

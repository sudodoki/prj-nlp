import nltk

#nouns, pronouns, adjectives, verbs, adverbs
cap_tags = ["NN", "NNP", "NNS", "NNPS", \
            "PRP", \
            "JJ", "JJR", "JJS", \
            "VB", "VBD", "VBG", "VBN", "VBP", "VBZ", \
            "RB", "RBR", "RBS"] 

sub_conj = ["after", "although", "as", "because", "before", "if", "once", "since","than", "that", "though",\
            "unless", "until", "when", "whenever", "where", "whereas", "wherever", "whether", "while", "why"]

def is_subconj(word):
    return (word.lower() in sub_conj)

def need_capitalize(word, tag):
    if tag in cap_tags and not word.isupper() or (tag == "IN" and is_subconj(word)):
        return True
    else:
        return False


tokenizer = nltk.tokenize.TreebankWordTokenizer()

src_filename = "examiner-headlines.txt"

dest_filename = "examiner-headlines-processed.txt"

line_count = 0

with open(dest_filename, mode="w", encoding="utf8") as  dest_file:
    with open(src_filename, mode="r", encoding="utf8") as src_file:
        for line in src_file:
            line_count += 1
    #        line = "Google giving a shiny gift to holiday travelers: free in-flight wi-fi although it's not right"
    #        print(line)
            tokens = tokenizer.tokenize(line)
            spans = tokenizer.span_tokenize(line)
            tags = nltk.pos_tag(tokens)
            i = 0
            for (sp, ep) in spans:    
                word = line[sp:ep]
                tag = tags[i]
                if need_capitalize(word, tags[i][1]):
                    word = word.title()
                    line = line[:sp] + word + line[ep:]
                i += 1
            
            dest_file.write(line)
#            print(line)

print("{} lines processed".format(line_count))




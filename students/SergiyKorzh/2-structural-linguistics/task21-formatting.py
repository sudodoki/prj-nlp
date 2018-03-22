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

#not all upper cases (abbreviation?) or already capitalized
def need_capitalize(word, tag):
   cap_word = word.title()
   need_cap = not word.isupper() \
       and (tag in cap_tags or (tag == "IN" and is_subconj(word))) \
       and cap_word != word

   return (need_cap,cap_word)


tokenizer = nltk.tokenize.TreebankWordTokenizer()

src_filename = "examiner-headlines.txt"

dest_filename = "examiner-headlines-formatted.txt"

line_count = 0
fixed_count = 0

with open(dest_filename, mode="w", encoding="utf8") as  dest_file:
    with open(src_filename, mode="r", encoding="utf8") as src_file:
        for line in src_file:
            line_count += 1
            tokens = tokenizer.tokenize(line)
            spans = tokenizer.span_tokenize(line)
            tags = nltk.pos_tag(tokens)
            i = 0
            fixed_title = False
            for (sp, ep) in spans:    
                word = line[sp:ep]
                (need_cap, cap_word) = need_capitalize(word, tags[i][1])
                if need_cap:
                    word = cap_word
                    line = line[:sp] + word + line[ep:]
                    fixed_title = True
                i += 1
            
            if fixed_title:
                fixed_count += 1

            dest_file.write(line)

print("{} lines processed, {} were fixed".format(line_count, fixed_count))
#5000 lines processed, 4261 were fixed




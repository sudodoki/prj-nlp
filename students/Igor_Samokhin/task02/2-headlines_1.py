import spacy
nlp = spacy.load('en_core_web_lg')

# additional sources on AP headline capitalization:
# https://headlinecapitalization.com/
# https://www.bkacontent.com/how-to-correctly-use-apa-style-title-case/
# that is why, for example, we also capitalize all words more than 4 characters long

# watch file location
with open('examiner-headlines.txt') as f:
    headlines = [h.strip() for h in f]

def first_and_last(res):
    # helper function to capitalize first and last words
    words = res.split(' ')
    first, last = words[0], words[-1]
    if first.islower():
        first = first.capitalize()
    if last.islower():
        last = last.capitalize()
    return ' '.join([first] + words[1:-1] + [last])
    
def AP_capitalize(h):
    # capitalize headline h according to AP rules
    cap_pos_list = ['ADJ', 'ADV','NOUN', 'NUM', 'PRON', 'PROPN', 'SCONJ', 'VERB']
    # for purposes of parsing, POS-tagger marks many pronouns/pronomial adjectives DET
    # for our purposes, they are pronouns/adjectives and should be capitalized
    always_cap = ['all', 'some']
    never_cap = ["n't"]
    if h.isupper():
        # if the whole title is in CAPS LOCK, we lowercase it
        h = h.lower()
    doc = nlp(h)
    words = []
    for token in doc:
        if sum(1 for c in token.text if c.isupper()) > 1:
            # to preserve words like 'NBA' and 'DJs' 
            words.append(token.text_with_ws)
        elif token.text in never_cap:
            words.append(token.text_with_ws.lower())
        elif (token.pos_ in cap_pos_list or token.dep_ == 'mark' 
              or token.text in always_cap or len(token.text) > 4):
            # dependency 'mark' is used for subordinating words and is catched better than SCONJ
            # from other sources it looks like all words longer than four letters are also capitalized
            words.append(token.text_with_ws.capitalize())
        else:
            words.append(token.text_with_ws.lower())
    res = ''.join(words)
    # solve hyphenated words:
    res = ' '.join([w.title() if '-' in w else w for w in res.split(' ')])
    # cap first and last:
    res = first_and_last(res)
    return res

def APize_headlines(headlines, filename):
    with open(filename, 'w') as f:
        for h in headlines:
            f.write(AP_capitalize(h)+'\n')
            
APize_headlines(headlines, 'formatted_examiner.txt')
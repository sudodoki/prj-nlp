import unicodedata

try:
    from urllib.request import urlopen
    from urllib.parse import urlencode
except:
    from urllib import urlopen, urlencode # Python 2

import json


text = "My skin crawled with anticipation all day as I watched the clock tick-tock. I was ready to be off. or I did not want to be at work. I only worked 12-7, but thoughts of being anywhere but the mall filled me so full they were spilling from my rims! [glasses joke] I guess you could say my eyes sparkled. Celebratory champagne. Today was going to be a good day. I was certain of it when I woke up. I was still certain of it when my car was full of steam from my rain covered seats mixing with the texas morning heat bath. So what if it smells like a wet dog, right? Gag me. I digress. things have been good lately. very good. I love that I am changing daily. I love the freedom I have been handed for simply giving myself to God. Things keep getting better and better. ! after work today, I went upstairs to the movie theatre. I stood in line and had every intention of finally seeing Napoleon Dynamite[spelling?] but for some reason, I turned around and left. probably because it was friday night and every 11 year old in north dallas was there and I didnt want my solo movie voyage to be ruined by the beeps and bloops of text messages sounding while braces faces kissed painfully in front of me.. I'll go to the matinee next week. it'll be less expensive, anyway."

q_age = 'https://api.textgain.com/1/age?'
q_gen = 'https://api.textgain.com/1/gender?'


def get_age(text):
    q = q_age + urlencode(dict(q=text, lang='en', key='***'))
    r = urlopen(q)
    r = r.read()
    r = r.decode('utf-8')
    r = json.loads(r)
    return r['age'], r['confidence']


def get_gender(text):
    q = q_gen + urlencode(dict(q=text, lang='en', key='***'))
    r = urlopen(q)
    r = r.read()
    r = r.decode('utf-8')
    r = json.loads(r)
    return r['gender'], r['confidence']


LEMMA_DIR = '/Users/admin/edu/NLP/practical_NLP_course/'
file1 = 'BNC_lemmafile5.txt'
file2 = 'BNC_lemmafile10.txt'
file3 = 'e_lemma.txt'

lemma_set = set([])

for file in ['BNC_lemmafile5.txt', 'BNC_lemmafile10.txt']:
    with open(LEMMA_DIR+file, 'r', encoding='utf-8') as f1:
        lines = f1.read().split('\n')
        for line in lines:
            try:
                lemma = line.split(maxsplit=1)[0].lower()
                lemma_set.add(lemma)
            except IndexError as e:
                print(file, e, '\n')
            except UnicodeDecodeError as e:
                print(e, '\n')


output_file = 'lemmas.txt'
with open(output_file, 'w') as out:
    for item in lemma_set:
        out.write("%s\n" % item)

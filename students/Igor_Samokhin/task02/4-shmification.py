import sys

def shmificate(word):
    # self-explanatory
    if word.startswith('shm') or word.startswith('schm') or len(word) < 3:
        return word
    else:
        vowels = list('aieuo')
        for i in range(len(word)):
            if word[i] in vowels:
                short = word[i:]
                break
        if 'sh' in word or 'sch' in word:
            res = 'sm' + short
        else:
            res = 'shm' + short
    return res

def shmificate_text(text):
    # shmificate every second word (or the only word) in text
    words = text.split(' ')
    res = []
    for i in range(len(words)):
        if (len(words) > 1) and (i % 2 == 0):
            res.append(words[i])
        else:
            shmord = shmificate(words[i])
            res.append(shmord)
    return ' '.join(res)

#print(shmificate_text('Data Science and Natural Language Processing'))

if __name__ == '__main__':
    text = ' '.join(sys.argv[1:])
    print(shmificate_text(text))
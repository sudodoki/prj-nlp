import spacy
  
nlp = spacy.load('en')

processed = 0
formatted = 0

saveFile = open('examiner-headlines-formatted.txt', 'w', encoding='utf8')

sourceFile = open('examiner-headlines.txt', 'r', encoding='utf8')
content = sourceFile.readlines()
sourceFile.close()

content = [x.strip() for x in content]

print('Processing started')

for inputStr in content:
    sentence = nlp(inputStr)
    lst = []
    first_added = False
    last_added = False

    for token in sentence:
        
        if not first_added:
            if token.pos_ != "PUNCT":
                lst.append(token.i)
                first_added = True
        
        if not last_added:
            if sentence[len(sentence) - 1 - token.i].pos_ != "PUNCT":
                lst.append(len(sentence) - 1 - token.i)
                last_added = True

        if token.tag_[0:2] in {'NN', 'PR', 'JJ', 'MD', 'VB', 'RB', 'RP', 'WR'}:
            lst.append(token.i)
            continue

        if token.tag_ == "IN" and token.dep_ == "mark":
            lst.append(token.i)
            continue

        if sentence[token.i-1].text_with_ws == "-":
            lst.append(token.i)
            lst.append(token.i - 2)        

    formattedStr = ""
                
    for token in sentence:
        if token.i in lst:
            formattedStr += str(token.text_with_ws)[0].upper() + str(token.text_with_ws)[1:len(token.text_with_ws)]
        else:
            formattedStr += str(token.text_with_ws).lower()

    processed +=1
    if inputStr != formattedStr:
        formatted +=1

    saveFile.write(formattedStr + '\n')

    if processed % 500 == 0:
        print(str(processed), 'processed')

saveFile.close()    

print("\nProcessed: {}\nFormatted: {}\n".format(str(processed), str(formatted)))

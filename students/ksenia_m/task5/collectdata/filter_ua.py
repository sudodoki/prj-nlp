from langdetect import detect
from langdetect.lang_detect_exception import LangDetectException
from random import shuffle

mark=0
comment = ""

def sort_ua():
    fout = open("comments_ua.txt", mode="w", encoding="utf-8")
    with open("comments_ua.txt", mode="r", encoding="utf-8") as f:
        for line in f.readlines():
            if ":::::" in line:
                mark = int(line[0:1])
            else:
                try:
                    lang = detect(line)
                    if lang != 'ru':
                        fout.write(str(mark) + ":")
                        fout.write(line)
                except LangDetectException:
                    pass
                #print("can't detect langauage of:" , line)


def prepare_dataset():
    f = open("comments_ua.txt", mode="r", encoding="utf-8")
    lines = f.readlines()
    shuffle(lines)
    threshold = int(len(lines) * 0.8)
    fout = open("train.txt", mode="w", encoding="utf-8")
    for line in lines[0: threshold]:
        fout.write(line)
    fout.close()
    fout = open("valid.txt", mode="w", encoding="utf-8")
    for line in lines[threshold]:
        fout.write(line)
    fout.close()
#sort_ua()
prepare_dataset()
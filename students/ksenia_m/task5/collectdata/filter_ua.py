from langdetect import detect
from langdetect.lang_detect_exception import LangDetectException
from random import shuffle

comment = ""


def sort_ua():
    fout = open("comments_ua.txt", mode="w", encoding="utf-8")
    with open("comments.txt", mode="r", encoding="utf-8") as f:
        counter = 0
        for line in f.readlines():
            try:
                lang = detect(line[6:])
                if not line.endswith("\n"):
                    line = line + '\n'
                if lang != 'ru':
                    counter += 1
                    fout.write(line)
                    print(counter)
            except LangDetectException:
                pas


#sort_ua()
#prepare_dataset();
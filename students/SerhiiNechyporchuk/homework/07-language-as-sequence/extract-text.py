import sys
import bz2
import en_core_web_lg
import re

file = '/Users/serhiinechyporhuk/courses/usenet/WestburyLab.NonRedundant.UsenetCorpus.txt.bz2'
max_docs = 10000
separator = '---END.OF.DOCUMENT---'


regexes = [r'(B(U|WA))?(HA)[^ ]+!+', r'(LO)+']


def remove_garbage(line):
    for regex in regexes:
        line = re.sub(regex, '', line)
    return line


def prepare_doc(nlp, doc):
    new_doc = []
    for line in doc[3:]:
        if line == '' or line.startswith('From:') or re.search(r'[<>|#@*]|--|[.\-_=~"]{3,}', line):
            continue
        elif line.startswith('--'):
            break
        else:
            new_doc.append(line)
    str_doc = " ".join(new_doc)
    str_doc = remove_garbage(str_doc)
    nlpdoc = nlp(str_doc)
    if len(list(nlpdoc.sents)) <= 1:
        return None
    else:
        return str_doc


def extract(input, output, skip, nlp):
    with bz2.open(output, 'at') as w:
        with bz2.open(input) as f:
            doc = ['']
            processed = 0
            for i, bline in enumerate(f):
                try:
                    if i < skip:
                        continue
                    #if processed > max_docs:
                    #    break
                    bline = bline.strip()
                    line = bline.decode('utf-8', errors='ignore')
                    if line == separator:
                        pdoc = prepare_doc(nlp, doc)
                        if pdoc:
                            w.write(pdoc + "\n")
                            processed += 1
                            if processed % 1000 == 0:
                                print(processed)
                        doc = []
                    else:
                        doc.append(line)
                except (KeyboardInterrupt, SystemExit):
                    print("Total: ", processed)
                    break


if __name__ == '__main__':
    print(sys.argv)
    input = sys.argv[1]
    output = sys.argv[2]
    skip = int(sys.argv[3])
    assert input and output and skip
    nlp = en_core_web_lg.load(disable=['parser', 'ner', 'textcat'])
    nlp.add_pipe(nlp.create_pipe('sentencizer'))
    extract(input, output, skip, nlp)
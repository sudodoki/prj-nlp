import sys
import re
import en_core_web_lg
from collections import Counter
import bz2
import rocksdb




if __name__ == '__main__':
    print(sys.argv)
    input = sys.argv[1]
    output = sys.argv[2]
    assert input and output
    nlp = en_core_web_lg.load(disable=['parser', 'ner', 'textcat', 'tagger'])
    nlp.add_pipe(nlp.create_pipe('sentencizer'))
    gather(input, output,  nlp)
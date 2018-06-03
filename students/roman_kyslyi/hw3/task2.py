#wikionary dump location:
#https://dumps.wikimedia.org/dewiktionary/20180320/dewiktionary-20180320-pages-articles-multistream.xml.bz2
from langdetect.lang_detect_exception import LangDetectException

INPUT_FILENAME = "dewiktionary-20180320-pages-articles-multistream.xml"
OUTPUT_FILENAME = "de_synonims.txt"
SYNONYM_LABEL = "{{Synonyme}}"
AFTER_SYNONYM = ["{{Beispiele}}","{{Gegenwörter}}","{{Oberbegriffe}}","{{Sinnverwandte","{{Unterbegriffe}}"]
from langdetect import DetectorFactory, detect
DetectorFactory.seed = 0
from lxml import etree

def get_synomyms(text): # щось адекватний regexp не вийшов :(
    words = text.split()
    syn = []
    is_syn = 0
    for word in words:
        if word== SYNONYM_LABEL: is_syn=1
        elif word in AFTER_SYNONYM: is_syn=0
        if is_syn == 1 and word != SYNONYM_LABEL and ':[' not in word and '{' not in word and '<' not in word: syn.append(word.replace('[','').replace(']','').replace(',,',','))
    return syn


def process(file, output):
    with open(output, 'w', encoding="utf-8") as f2:
        events = ("start", "end")
        context = etree.iterparse(file, events=events)
        for action, elem in context:
            if action == "start" and "title" in elem.tag:
                title = elem.text
            if action == "start" and "text" in elem.tag:
                if elem.text is not None and elem.text.find('{{Synonyme}}'):
                    synonyms = get_synomyms(elem.text)
                    if synonyms:
                        f2.write("{}: {}{}".format(title, ", ".join(synonyms),'\n'))



process(INPUT_FILENAME,OUTPUT_FILENAME)
#result in desynonims.txt
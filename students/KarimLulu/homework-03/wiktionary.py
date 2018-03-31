import xml.sax
from xml.sax import make_parser
import bz2
import re
from collections import defaultdict
import sys

from config import wiki_filename, wiki_output, data_dir

class WikiHandler(xml.sax.handler.ContentHandler):

    def __init__(self, patt_raw, patt_syns):
        self.patt_raw = patt_raw
        self.patt_syns = patt_syns
        self.inPage, self.inTitle, self.inText = [False] * 3
        self.data = defaultdict(list)

    def find_synonyms(self, text):
        match = re.search(self.patt_raw, text, re.M)
        if match:
            raw_syns = match.group(1)
            return re.findall(self.patt_syns, raw_syns, re.M)
        return []

    def startElement(self, name, attrs):
        if name == "page":
            self.inPage = True
        elif name == "title":
            if self.inPage:
                self.inTitle = True
                self.title = ""
        elif name == "text":
            if self.inPage:
                self.inText = True
                self.text = ""

    def endElement(self, name):
        if name == "page":
            self.inPage = False
        elif name == "title":
            self.inTitle = False
        elif name == "text":
            self.inText = False
            synonyms = self.find_synonyms(self.text)
            if synonyms:
                self.data[self.title].extend(synonyms)

    def characters(self, content):
        if self.inTitle:
            self.title = self.title + content
        if self.inText:
            self.text = self.text + content

    def endDocument(self):
        with (data_dir / wiki_output).open("w+") as f:
            for key, value in self.data.items():
                line = f"{key}: " + ", ".join(value) + "\n"
                f.write(line)

def main():
    patt_raw = r"(?:СИНОНИМИ =\s+([^|]*))"
    patt_syns = r"\[\[(.*?)\]\]"
    handler = WikiHandler(patt_raw, patt_syns)
    parser = make_parser()
    parser.setContentHandler(handler)
    with bz2.open(data_dir / wiki_filename, mode="rt") as f:
        parser.parse(f)
    return 0

if __name__ == "__main__":
    code = main()
    sys.exit(code)

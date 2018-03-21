import xml.sax
import tarfile
import json
import sys

from config import data_dir, annotated_data, annotations_output

PATT = "sgml"
TYPE = "noalt"

class Handler(xml.sax.handler.ContentHandler):

    def __init__(self):
        self.inType, self.inCorrection = [False] * 2
        self.data = []

    def add_mistake(self):
        mistake = {"teacher": self.teacher_id,
                   "doc_id": self.doc_id,
                   "start_par": self.start_par,
                   "start_off": self.start_off,
                   "end_par": self.end_par,
                   "end_off": self.end_off,
                   "type": self.type,
                   "correction": self.correction}
        self.data.append(mistake)

    def startElement(self, name, attrs):
        if name == "DOC":
            self.doc_id = attrs.get("nid")
        elif name == "ANNOTATION":
            self.teacher_id = attrs.get("teacher_id")
        elif name == "MISTAKE":
            self.start_par = attrs.get("start_par")
            self.start_off = attrs.get("start_off")
            self.end_par = attrs.get("end_par")
            self.end_off = attrs.get("end_off")
        elif name == "TYPE":
            self.inType = True
            self.type = ""
        elif name == "CORRECTION":
            self.inCorrection = True
            self.correction = ""

    def endElement(self, name):
        if name == "TYPE":
            self.inType = False
        elif name == "CORRECTION":
            self.inCorrection = False
            self.add_mistake()

    def characters(self, content):
        if self.inType:
            self.type = self.type + content
        if self.inCorrection:
            self.correction = self.correction + content

    def endDocument(self):
        with (data_dir / annotations_output).open("w+") as f:
            json.dump(self.data, f, indent=4)

def main():
    handler = Handler()
    with tarfile.open(data_dir / annotated_data) as tar:
        filenames = list(filter(lambda x: PATT in x and TYPE in x, tar.getnames()))
        for filename in filenames:
            f = tar.extractfile(filename).read()
            f = b"<root>" + f + b"</root>"
            xml.sax.parseString(f, handler)
    return 0

if __name__ == "__main__":
    code = main()
    sys.exit(code)

import xml.sax
import tarfile
import json
import sys
from itertools import groupby
from operator import itemgetter

from config import data_dir, annotated_data, annotations_output

PATT = "sgml"
TYPE = "noalt"

def distance(a, b, start="start_off", end="end_off"):
    return max(abs(a[start]-b[start]), abs(a[end]-b[end]))

def closeness(a, b, start="start_off", end="end_off"):
    IOU = (min(a[end], b[end]) - max(a[start], b[start])) / (max(a[end], b[end]) - min(a[start], b[start]))
    if IOU < 0:
        return 0
    return IOU

def estimate_agreement(data):
    k = 0
    for doc_id, doc_group in groupby(sorted(data, key=itemgetter("doc_id")), key=itemgetter("doc_id")):
        for par_num, par_group in groupby(sorted(doc_group, key=itemgetter("start_par")), key=itemgetter("start_par")):
            chunk = sorted(par_group, key=itemgetter("teacher"))
            split = [list(gr) for key, gr in groupby(chunk, key=itemgetter("teacher"))]
            if len(split) > 1:
                teacher_1, teacher_2 = split
                for correction in teacher_1:
                    closest_correction = min(teacher_2, key=lambda x: distance(x, correction))
                    metric = closeness(correction, closest_correction)
                    k += metric
            else:
                pass
    return k * 100.0 / (len(data) - k)

def estimate_agreement_by_type(data):
    split_by_type = [(key, list(gr)) for key, gr in groupby(sorted(data, key=itemgetter("type")),
                                                            key=itemgetter("type"))]
    output = {}
    for key, chunk in split_by_type:
        output[key] = estimate_agreement(chunk)
    return output

class Handler(xml.sax.handler.ContentHandler):

    def __init__(self):
        self.inType, self.inCorrection = [False] * 2
        self.data = []

    def add_mistake(self):
        mistake = {"teacher": int(self.teacher_id),
                   "doc_id": int(self.doc_id),
                   "start_par": int(self.start_par),
                   "start_off": int(self.start_off),
                   "end_par": int(self.end_par),
                   "end_off": int(self.end_off),
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
        agreement = estimate_agreement_by_type(self.data)
        print(json.dumps(agreement, indent=4))

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

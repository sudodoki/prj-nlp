import xml.sax
import tarfile
import json
import sys
from itertools import groupby, product
from operator import itemgetter
import logging

from config import (data_dir, annotated_data, agreement_output, annotations_output,
                    date_fmt, log_fmt)

logging.basicConfig(level=logging.INFO, 
                    format=log_fmt,
                    datefmt=date_fmt)
logger = logging.getLogger(__name__)

PATT = "sgml"
TYPE = "noalt"
ROUND = 3

def distance(a, b, start="start_off", end="end_off"):
    return max(abs(a[start]-b[start]), abs(a[end]-b[end]))

def IOU(a, b, start="start_off", end="end_off"):
    intersection = min(a[end], b[end]) - max(a[start], b[start])
    if intersection < 0:
        return 0
    union = max(a[end], b[end]) - min(a[start], b[start])
    return intersection / union

def estimate_agreement(data):
    intersection = 0
    for doc_id, doc_group in groupby(sorted(data, key=itemgetter("doc_id")), 
                                     key=itemgetter("doc_id")):
        for par_num, par_group in groupby(sorted(doc_group, key=itemgetter("start_par")), 
                                          key=itemgetter("start_par")):
            chunk = sorted(par_group, key=itemgetter("teacher"))
            split = [list(gr) for key, gr in groupby(chunk, key=itemgetter("teacher"))]
            if len(split) > 1:
                for x, y in product(*split): #y = min(teacher_2, key=lambda x: distance(x, correction))
                        intersection += IOU(x, y)
            else:
                pass
    return intersection * 100.0 / (len(data) - intersection)

def estimate_agreement_by_type(data):
    split_by_type = [(key, list(gr)) for key, gr in groupby(sorted(data, key=itemgetter("type")),
                                                            key=itemgetter("type"))]
    output = {}
    for key, chunk in split_by_type:
        output[key] = estimate_agreement(chunk)
    return output

class Handler(xml.sax.handler.ContentHandler):

    def __init__(self):
        self.inType = self.inCorrection = False
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

def main():
    handler = Handler()
    with tarfile.open(data_dir / annotated_data) as tar:
        filenames = list(filter(lambda x: PATT in x and TYPE in x, tar.getnames()))
        for filename in filenames:
            f = tar.extractfile(filename).read()
            f = b"<root>" + f + b"</root>"
            xml.sax.parseString(f, handler)
    logger.info("Parsed data")
    # Dump parsed annotations
    with (data_dir / annotations_output).open("w+") as f:
        json.dump(handler.data, f, indent=4)
    # Estimate and dump agreement
    with (data_dir / agreement_output).open("w+") as f:
        for key, value in estimate_agreement_by_type(handler.data).items():
            line = f"{key}: {value:0.{ROUND}f}%"
            f.write(line + "\n")
            logger.info(line)
        agreement = estimate_agreement(handler.data)
        line = f"Total: {agreement:0.{ROUND}f}%"
        f.write("\n" + line + "\n")
        logger.info(line)
    return 0

if __name__ == "__main__":
    code = main()
    sys.exit(code)

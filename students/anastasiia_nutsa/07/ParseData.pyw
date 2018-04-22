from xml.sax import make_parser
from xml.sax.saxutils import XMLFilterBase

class QAExtractor(XMLFilterBase):
    def __init__(self, parent=None):
        super().__init__(parent)
        self._buffer = []
        self._is_qa = False
        self._processed = 0
        self._out = open('data.txt', 'w', encoding='utf-8')

    def _flushCharBuffer(self):
        s = ''.join(self._buffer)
        s = s.replace("<br>", " ").replace("<br />", " ").replace("\t", " ")
        s = s.replace("\r", " ").replace("\n", " ").replace("\r\n", " ")
        while "  " in s:
            s = s.replace("  ", " ")
        self._out.write("{}\n".format(s))
        self._buffer = []
        self._processed += 1
        if self._processed % 1000 == 0:
            print("Processed: " + str(self._processed))

    def startElement(self, name, attrs):
        self._is_qa = True if name in ('content', 'bestanswer') else False

    def characters(self, data):
        if self._is_qa:
            self._buffer.append(data)

    def endElement(self, name):
        if name == 'content':
            self._buffer.append(' ')
        if name == 'bestanswer':
            self._flushCharBuffer()

    def endDocument(self):
        self._out.close()
        print("Processed: " + str(self._processed))

reader = QAExtractor(make_parser())
reader.parse('manner.xml')

print("DONE")

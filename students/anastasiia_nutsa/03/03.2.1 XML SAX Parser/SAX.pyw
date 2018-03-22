import re
from xml.sax import make_parser
from xml.sax.saxutils import XMLFilterBase

class SynFilter(XMLFilterBase):
    def __init__(self, parent=None):
        super().__init__(parent)
        self._titleBuffer = []
        self._textBuffer = []
        self._processed = 0
        self._result = ""
        self._is_title = False
        self._is_text = False
        self._out = open('output.txt', 'w', encoding='utf8')

    def _flushCharBuffer(self):
        if self._is_title:
            s = ''.join(self._titleBuffer)
            self._titleBuffer = []
        if self._is_text:
            s = ''.join(self._textBuffer)
            self._textBuffer = []
            synSections = re.findall(r'\{\{Синоним(?s).*?\}\}', s)
            if synSections:
                for synSection in synSections:
                    synList = re.findall(r'\[\[[^.:|]*?\]\]', synSection)
                    for syn in synList:
                        if not self._result.endswith(":"):
                            self._result += " |"
                        self._result += " " + syn.replace("[", "").replace("]", "")
                if not self._result.endswith(":"):                        
                    self._out.write(self._result + "\n")                
                    self._processed += 1
                    if self._processed % 1000 == 0:
                        print("Processed: " + str(self._processed))
                self._result = ""    
                    
        return s.strip()

    def startElement(self, name, attrs):
        if name == 'title':
            self._is_title = True
        else:
            self._is_title = False
            
        if name == 'text':
            self._is_text = True
        else:
            self._is_text = False    

    def characters(self, data):
        if self._is_title:
            self._titleBuffer.append(data)
        if self._is_text:
            self._textBuffer.append(data)

    def endElement(self, name):
        if self._is_title:
            self._result = self._flushCharBuffer() + " :"
        if self._is_text:
            self._flushCharBuffer()            
        #if self._processed == 100:
        #    exit()

    def endDocument(self):
        self._out.close()
        print("Processed: " + str(self._processed))
            
reader = SynFilter(make_parser())
reader.parse("srwiktionary-20180301-pages-meta-current.xml")

print("DONE")

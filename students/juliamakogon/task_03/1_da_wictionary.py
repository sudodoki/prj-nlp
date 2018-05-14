'''
Process Wiktionary dump to extract synonym relations for a random language 
(not English, Ukrainian or Russian :) - requires application of XML SAX parsing
'''

from xml.sax.handler import ContentHandler
from xml.sax import make_parser
import os
import re

class SinHandler(ContentHandler):
    def __init__(self):
        ContentHandler.__init__( self)
        self.sins = {}
        self.currentTag = None
        self.pagecount = 0
        self.stop = False
        pass
    
    def skipWord(self):
        return self.word.find(':') > 0
    
    def setWord(self, content):
        self.word = content
        self.skip = self.skipWord()     

            
    
    def initPage(self):
        self.skip = False
        self.word = ''
        self.section = ''
        
    
    def lookForSins(self, line):
        if line.startswith('=={{limba') and not line.startswith('=={{limba|ron}}'):
            self.skip = True
            return
        
        matchObj = re.match( r'{{-(.*)-}}', line)
        if matchObj:
            self.section = matchObj.group(1)
            return

        if self.section == 'sin':
            if line.strip() and not (line.startswith('*') or line.startswith('#')):
                self.section = ''
                return

            mo = re.findall('\[\[(.*?)\]\]', line)
            if mo:
                if not self.word in self.sins:
                    self.sins[self.word] = set()
                c = self.sins[self.word]                
                [c.add(m) for m in mo]


        
    def startElement(self, name, attrs):
        self.currentTag = name
        if name == 'page':
            self.initPage()

            
    def endElement(self,name):
        self.currentTag = None
        if name == 'page':
            self.pagecount += 1

    def characters(self, content):
       
        if self.currentTag == 'title':
            self.setWord(content)
        elif self.currentTag == 'text' and not self.skip:
            self.lookForSins(content)


PAGELIMIT = 10000
h = SinHandler()
saxparser = make_parser()
saxparser.setContentHandler(h)

path = r'C:\\work\\jul\\py_ml\\rowiktionary-20180301-pages-articles.xml\\'
fname = r'rowiktionary-20180301-pages-articles.xml'


with open(os.path.join(path, fname),"r", encoding="utf-8") as stream:
    line = stream.readline()
    while line: # and h.pagecount < PAGELIMIT:
        saxparser.feed(line)
        line = stream.readline()


print(h.pagecount)
with open("rowiktionary_sin.txt", "w", encoding="utf-8") as f:
    for w in h.sins:
        s = "{}: {}".format(w, ", ".join(h.sins[w]))
        print(s)
        f.write(s + '\n')



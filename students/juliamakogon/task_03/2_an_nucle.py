from xml.sax.handler import ContentHandler
from xml.sax import make_parser
import os

class Mistake():
    def __init__(self):
        self.start_par = 0
        self.start_off = 0
        self.end_par = 0
        self.end_off = 0
        self.type_ = ''
        self.correction = ''
        pass

class Agreement():
    def __init__(self):
        self.ann1 = None
        self.ann2 = None
        self.document_count = 0
        self.count_mistakes_total = 0
        self.count_agreed_total = 0
        self.precision_sum = 0
        self.textlen_sum = 0
        self.erchar1_sum = 0
        self.erchar2_sum = 0
        self.erchar_a_sum = 0
   
    def kappa(erchar1, erchar2, erchar_a, textlen):
        po = 1 + (2*erchar_a - erchar1 - erchar2)/textlen # precision of by-char agreement
        pe = erchar1*erchar2/textlen/textlen #probability of random agreement
        kappa = (po - pe)/(1 - pe) # https://en.wikipedia.org/wiki/Cohen%27s_kappa
        return kappa
    
    def compare_annotations(self):
        if self.ann1.nid != self.ann2.nid:
            raise Exception("Something unbelievable! self.ann1.nid != self.ann2.nid")
#        print('compare_annotations', self.ann1.nid)
        textlen = max(self.ann1.textlen, self.ann2.textlen)
        self.document_count += 1
        i1 = 0
        i2 = 0
        mis1 = self.ann1.mistakes
        mis2 = self.ann2.mistakes
        erchar1 = 0 # number of chars annotator 1 marked as error
        erchar2 = 0
        erchar_a = 0 # number of chars where 1 and 2 agreed
        count_mistakes = 0
        count_agreed = 0
        while i1 < len(mis1) and i2 < len(mis2):
            count_mistakes += 1
            if mis1[i1].end_par < mis2[i2].start_par:
                # annotator 2 skipped the error 
                erchar1 += mis1[i1].end_off - mis1[i1].start_off
                i1 += 1
                pass
            elif mis1[i1].start_par > mis2[i2].end_par:
                # annotator 1 skipped the error
                erchar2 += mis2[i2].end_off - mis2[i2].start_off
                i2 += 1                
                pass
            elif mis1[i1].end_off < mis2[i2].start_off:
                # annotator 2 skipped the error
                erchar1 += mis1[i1].end_off - mis1[i1].start_off
                i1 += 1                
            elif mis1[i1].start_off > mis2[i2].end_off:
                # annotator 1 skipped the error
                erchar2 += mis2[i2].end_off - mis2[i2].start_off
                i2 += 1                  
            else:
                count_agreed += 1
                erchar1 += mis1[i1].end_off - mis1[i1].start_off
                erchar2 += mis2[i2].end_off - mis2[i2].start_off
                erchar_a += min(mis2[i2].end_off, mis1[i1].end_off) - max(mis2[i2].start_off, mis1[i1].start_off)
                i1 += 1
                i2 += 1
        pr = count_agreed/count_mistakes
        kappa_val = Agreement.kappa(erchar1, erchar2, erchar_a, textlen)
        print("nid = {}\tannotator1 = {}\tannotator2 = {}\tmistakes = {}\tagreed = {}\tprecision = {}\tkappa = {}"
              .format(self.ann1.nid, len(mis1), len(mis2), count_mistakes, count_agreed, pr, kappa_val))
        self.count_mistakes_total +=  count_mistakes
        self.count_agreed_total += count_agreed
        self.precision_sum += pr
        self.textlen_sum += textlen
        self.erchar1_sum += erchar1
        self.erchar2_sum += erchar2
        self.erchar_a_sum += erchar_a
    
    def add_annotation(self, ann):
        if not self.ann1:
            self.ann1 = ann
        elif not self.ann2:
            self.ann2 = ann
            self.compare_annotations()
        else:
            self.ann1 = ann
            self.ann2 = None
        pass
    
    def print(self):
        print("Documents compared:", self.document_count)
        print("Total mistakes:", self.count_mistakes_total)
        print("Total agreed:", self.count_agreed_total)
        print("Overall precision (probability that error marked by both annotators):", self.count_agreed_total / self.count_mistakes_total)
        print("Average precision for the doc:", self.precision_sum / self.document_count)
        print("Annotators agreement (by-char Cohen's kappa):", Agreement.kappa(self.erchar1_sum, self.erchar2_sum, self.erchar_a_sum, self.textlen_sum))

class Annotation():
    def __init__(self):
        self.nid = 0
        self.teacher_id = 0 
        self.mistakes = []
        self.textlen = 0
    
    def add_mistake(self, mistake):
        self.mistakes.append(mistake)
        pass
    
class DocHandler(ContentHandler):
    def __init__(self, agreement):
        ContentHandler.__init__( self)
        self.agreement = agreement
        self.ann = None
        self.mistake = None
        self.currentTag = None
        pass
        
    def startElement(self, name, attrs):
        self.currentTag = name
        if name == 'DOC':
            self.ann = Annotation()
            self.ann.nid = attrs.get('nid')
        elif name == 'ANNOTATION':
            self.ann.teacher_id = attrs.get('teacher_id')
        elif name == 'MISTAKE':
            self.mistake = Mistake()
            self.mistake.start_par = int(attrs.get('start_par'))
            self.mistake.start_off = int(attrs.get('start_off'))
            self.mistake.end_par = int(attrs.get('end_par'))
            self.mistake.end_off = int(attrs.get('end_off'))
            
    def endElement(self,name):
        self.currentTag = None
        if name == 'DOC':
            self.agreement.add_annotation(self.ann)
        elif name == 'MISTAKE':
            self.ann.add_mistake(self.mistake)

    def characters(self, content):
        if not self.mistake:
            return
        
        if self.currentTag == 'TYPE':
            self.mistake.type_ = content
        elif self.currentTag == 'CORRECTION':
            self.mistake.correction = content
        elif self.currentTag == 'P' or self.currentTag == 'TITLE':
            self.ann.textlen += len(content)



try:
    agreement = Agreement()
    doc = DocHandler(agreement)
    saxparser = make_parser()
    saxparser.setContentHandler(doc)
    
    path = r'C:\\work\\jul\\py_ml\\conll14st-test-data\\noalt'
    fnames = ['official-2014.0.sgml', 'official-2014.1.sgml']
    streams = []

    for fname in fnames:
        streams.append(open(os.path.join(path, fname),"r"))
    eof = False
    while not eof:
        for stream in streams:
            line = stream.readline()
            saxparser.reset()
            while line:
                saxparser.feed(line)
                if line.strip() == '</DOC>':
                    saxparser.close()
                    break
                line = stream.readline()
            if not line:
                eof = True

    agreement.print()
                
except Exception as error:
    print(error)
finally:
    for stream in streams:
        stream.close()

    


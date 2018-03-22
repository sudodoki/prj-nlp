"""
Process data from the [NUCLE Error Corpus]
and analyze inter-annotator agreement in it (general and for each error type).
"""
import os
import zipfile
from collections import Counter
from bs4 import BeautifulSoup
from students.oleg_m.utils.stack import Stack


class ComparisonDoc(object):
    """
    Object for compare 2 docs wuth mistakes by different teachers

    Attributes:
        doc_id (int): id of the doc
        this_doc (Doc): the doc to compare with
        other_doc (Doc): comparable doc
        mistake_stats (Counter): mistake stats

    """

    def __init__(self, this_doc, other_doc):
        self.doc_id = int(this_doc.attrs.get('nid', 0))
        self.this_doc = this_doc
        self.other_doc = other_doc
        self.mistake_stats = Counter()

    @staticmethod
    def create_stack(doc):
        """
        prepare stack of corrected mistakes from the doc
        :param doc: doc object
        :return: stack of the docs (first in the top)
        """
        temp_stack = Stack()
        annotation = doc.annotation
        mistakes = annotation.find_all('mistake')
        for mistake in reversed(mistakes):
            temp_stack.push(Mistake(mistake))
        return temp_stack

    def compare_mistakes(self):
        """
        Compare 2 stacks of mistakes.
        The difference is added to the counter with specific mistake
        In one correction, few types of mistake can be found
        """
        this_stack = self.create_stack(self.this_doc)
        other_stack = self.create_stack(self.other_doc)
        while not this_stack.isEmpty() and not other_stack.isEmpty():
            this_mstk = this_stack.peek()
            other_mstk = other_stack.peek()
            if this_mstk == other_mstk:
                # if mistakes are corrected in one way
                this_stack.pop()
                other_stack.pop()
                self.mistake_stats['same'] += 1
            elif this_mstk.is_same_span(other_mstk):
                # mistakes with the same span
                if not this_mstk.is_same_type(other_mstk):
                    self.mistake_stats['type'] += 1
                if not this_mstk.is_same_correction(other_mstk):
                    self.mistake_stats['correction'] += 1
                this_stack.pop()
                other_stack.pop()
            elif this_mstk.is_span_intersects(other_mstk):
                # mistakes with the spans that intersects
                self.mistake_stats['span'] += 1
                if not this_mstk.is_same_type(other_mstk):
                    self.mistake_stats['type'] += 1
                if not this_mstk.is_same_correction(other_mstk):
                    self.mistake_stats['correction'] += 1
                this_stack.pop()
                other_stack.pop()
            elif this_mstk.is_mistake_before(other_mstk):
                # only this teacher corrected mistake
                self.mistake_stats['this_miss'] += 1
                this_stack.pop()
            elif other_mstk.is_mistake_before(this_mstk):
                # only other teacher corrected mistake
                self.mistake_stats['other_miss'] += 1
                other_stack.pop()
            else:
                # debug block
                print('Something new')
                print(this_stack.peek())
                print(other_stack.peek())
                break

        while not this_stack.isEmpty():
            # if mistakes of this teacher left
            self.mistake_stats['this_miss'] += 1
            this_stack.pop()

        while not other_stack.isEmpty():
            # if mistakes of other teacher left
            self.mistake_stats['other_miss'] += 1
            other_stack.pop()

    def get_id(self):
        """
        get id of the doc
        :return: doc_id
        """
        return self.doc_id

    def get_stats(self):
        """
        get counter of mistakes by the doc
        :return: (Counter) counter of mistakes
        """
        return self.mistake_stats

    def __str__(self):
        return self.mistake_stats


class Mistake(object):
    """
    Object of mistake with function for comparison

    Attributes:
        mistake_type (str): type of annotated mistake
        start_part (int): start text number of the mistake
        end_part (int): end text number of the mistake
        start_pos (int): start number of position in the text of the mistake
        end_pos (int): end number of position in the text of the mistake
        correction (str): corrected value
    """

    def __init__(self, mistake):
        self.mistake_type = mistake.type.text
        self.start_part = int(mistake.attrs.get('start_par', 0))
        self.end_part = int(mistake.attrs.get('end_par', 0))
        self.start_pos = int(mistake.attrs.get('start_off', 0))
        self.end_pos = int(mistake.attrs.get('end_off', 0))
        self.correction = mistake.correction.text

    def is_mistake_before(self, other):
        """
        Is current mistake is before other mistake in the text (not intersects)
        :param other: mistake to compare with
        :return: bool True or False
        """
        if self.end_part < other.start_part:
            return True
        if self.end_part == other.start_part and self.end_pos <= other.start_pos:
            return True
        return False

    def is_same_span(self, other):
        """
        Is current mistake has the same span in the text with other mistake
        :param other: mistake to compare with
        :return: bool True or False
        """
        if (self.end_part == other.end_part and self.start_part == other.start_part
                and self.end_pos == other.end_pos and self.start_pos == other.start_pos):
            return True
        return False

    def is_span_intersects(self, other):
        """
        Is current mistake intersects other mistake in the text
        :param other: mistake to compare with
        :return: bool True or False
        """
        if self.end_part == other.end_part and self.start_part == other.start_part:
            if self.start_pos == other.start_pos or self.end_pos == other.end_pos:
                return True
            elif self.start_pos < other.end_pos and self.end_pos > other.start_pos:
                return True
        return False

    def is_same_type(self, other):
        """
        Are mistakes have the same type of error
        :param other: mistake to compare with
        :return: bool True or False
        """
        return self.mistake_type == other.mistake_type

    def is_same_correction(self, other):
        """
        Are mistakes have the same corrected value
        :param other: mistake to compare with
        :return: bool True or False
        """
        return self.correction == other.correction

    def __eq__(self, other):
        return all([self.mistake_type == other.mistake_type,
                    self.start_part == other.start_part,
                    self.end_part == other.end_part,
                    self.start_pos == other.start_pos,
                    self.end_pos == other.end_pos,
                    self.correction == other.correction])

    def __ne__(self, other):
        return any([self.mistake_type != other.mistake_type,
                    self.start_part != other.start_part,
                    self.end_part != other.end_part,
                    self.start_pos != other.start_pos,
                    self.end_pos != other.end_pos,
                    self.correction != other.correction])

    def __str__(self):
        return str(self.__dict__)


# read zip file with WikiDict xml
zf = zipfile.ZipFile('files/official-2014.zip', 'r')
teacher_8_file = zf.extract('official-2014.0.sgml', 'files/')
teacher_9_file = zf.extract('official-2014.1.sgml', 'files/')

with open('files/official-2014.0.sgml', 'r', encoding='utf-8') as f:
    soup_0 = BeautifulSoup(f, 'html.parser')

with open('files/official-2014.1.sgml', 'r', encoding='utf-8') as f:
    soup_1 = BeautifulSoup(f, 'html.parser')

# two objects of different teachers
teacher_8 = soup_0.find_all('doc')
teacher_9 = soup_1.find_all('doc')
doc_len = len(teacher_8)

# counter for total stats
stats_total = Counter()
# counter by mistakes
stats_by_doc = dict()

for i in range(doc_len):
    # create comparison object
    comparison = ComparisonDoc(teacher_8[i], teacher_9[i])
    comparison.compare_mistakes()
    mistake_stats = comparison.get_stats()
    stats_by_doc[comparison.get_id()] = mistake_stats
    # aggregate mistakes
    stats_total += mistake_stats

print('doc #1: ', stats_by_doc.get(1))
print('doc #10: ', stats_by_doc.get(10))
print('doc #15: ', stats_by_doc.get(15))
print('doc #25: ', stats_by_doc.get(25))
print('doc #40: ', stats_by_doc.get(40))
print('doc #50: ', stats_by_doc.get(50))
print('total: ', stats_total)

try:
    os.remove('files/official-2014.0.sgml')
    os.remove('files/official-2014.1.sgml')
except OSError:
    pass

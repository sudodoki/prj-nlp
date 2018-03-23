# coding: utf-8

from lxml import etree
import tarfile

tar = tarfile.open("conll14st-test-data.tar.gz")

set1 = tar.getmembers()[12] # conll14st-test-data/noalt/official-2014.0.sgml
set2 = tar.getmembers()[14] # conll14st-test-data/noalt/official-2014.1.sgml

def debug_events(parser):
    for action, element in parser.read_events():
        print('%s: %s %s %s' % (action, element.tag, element.attrib, element.text))

def file_to_annotation_docs(fileObj):
    parser = etree.XMLPullParser(events=('start', 'end'))    
    docs = []
    currentDoc = None
    currentAnnotation = None
    currentMistake = None
    currentText = ''
    # work around multiple roots in original doc
    parser.feed(b'<EVERYTHING>\n')
    for line in fileObj.readlines():        
        parser.feed(line)
        for action, element in parser.read_events():
            if (action == 'start' and element.tag == 'DOC'):
                currentDoc = {
                    'currentText': '',
                    'nid': element.attrib['nid']
                }
                currentText = ''
                continue
            if (action == 'end' and element.tag == 'DOC'):
                currentDoc['currentText'] = currentText
                docs.append(currentDoc)
                currentDoc = None
                continue
            if (action == 'start' and element.tag == 'TEXT'):
                continue
            if (action == 'end' and element.tag in ['TITLE', 'P']):
                currentText += element.text
                continue
            if (action == 'start' and element.tag == 'ANNOTATION'):
                currentAnnotation = {
                    'teacher_id': element.attrib['teacher_id'],
                    'mistakes': []
                }
                currentDoc['annotation'] = currentAnnotation
                continue
            if (action == 'start' and element.tag == 'MISTAKE'):
                currentMistake = {
                    'start': str(int(element.attrib['start_par']) * 1000) + element.attrib['start_off'],
                    'end': str(int(element.attrib['end_par']) * 1000) + element.attrib['end_off']
                }
                continue
            if (action == 'end' and element.tag == 'MISTAKE'):
                currentAnnotation['mistakes'].append(currentMistake)
                continue
            if (action == 'end' and element.tag == 'TYPE'):
                currentMistake['type'] = element.text
                continue
            if (action == 'end' and element.tag == 'CORRECTION'):
                currentMistake['corr'] = element.text
                continue         
    parser.feed(b'</EVERYTHING>\n')
    
    return docs 

docset1 = file_to_annotation_docs(tar.extractfile(set1))
docset2 = file_to_annotation_docs(tar.extractfile(set2))
print("Done")

def location_equal(loc1, loc2):
    return loc1['start'] == loc2['start'] and loc1['end'] == loc2['end']

def location_includes(loc1, loc2):
    return int(loc1['start']) >= int(loc2['start']) and int(loc1['end']) <= int(loc2['end']) or  int(loc2['start']) >= int(loc1['start']) and int(loc2['end']) <= int(loc1['end'])

def key_equals(key):
    return lambda obj1, obj2: obj1[key] == obj2[key]

def hashable_error(e, include_type=False, include_corr=False):
    res = {
        'start': e['start'],
        'end': e['end']
    }
    if include_type:
        res['type'] = e['type']
    if include_corr:
        res['corr'] = e['corr']
    return frozenset(res.items())

def check_agreement(error_set1, error_set2, include_type=False, include_corr=False, location_check=location_equal):
    mistakes1, mistakes2 = error_set1['mistakes'], error_set2['mistakes']
    intersection = set()
    union = set()
    for mistake_set in [mistakes1, mistakes2]:
        for mistake in mistake_set:
            same = [other for other in mistakes2 if location_check(mistake, other)]
            different = []
            if include_type:
                new_same = [other for other in same if key_equals('type')(mistake, other)]
                new_different = [other for other in same if not key_equals('type')(mistake, other)]
                same = new_same
                different = new_different
            if include_corr:
                new_same = [other for other in same if key_equals('corr')(mistake, other)]
                new_different = [other for other in same if not key_equals('corr')(mistake, other)]
                same = new_same
                different = different + new_different
            for e in same:
                intersection.add(hashable_error(e, include_type, include_corr))
            union.add(hashable_error(mistake, include_type, include_corr))
            for e in different:
                union.add(hashable_error(e, include_type, include_corr))
    return intersection, union

def get_agreement(docset1, docset2, include_type=False, include_corr=False, location_check=location_equal):
    total_interesction = 0
    total_union = 0
    for doc1, doc2 in zip(docset1, docset2):
        intersection, union = check_agreement(doc1['annotation'], doc2['annotation'], include_type, include_corr, location_check)
        total_interesction += len(intersection)
        total_union += len(union)
    agreement = total_interesction / total_union * 100
    return agreement

for include_type in (True, False):
    for include_corr in (True, False):
        for location_check in (location_equal, location_includes):
            banner = 'Agremeent with'
            if location_check == location_equal:
                banner += ' exactly same location '
            else:
                banner += ' location included in other location '
                
            banner += ' same type ' if include_type else ' ignoring types '
            banner += ' same correction ' if include_corr else ' ignoring correction '
            banner += ' agremenet is '
            print(banner, get_agreement(docset1, docset2, include_type=include_type, include_corr=include_corr, location_check=location_check), '%')

# Agremeent with exactly same location  same type  same correction  agremenet is  65.75207264113699 %
# Agremeent with location included in other location  same type  same correction  agremenet is  65.75207264113699 %
# Agremeent with exactly same location  same type  ignoring correction  agremenet is  67.77212614445574 %
# Agremeent with location included in other location  same type  ignoring correction  agremenet is  67.77212614445574 %
# Agremeent with exactly same location  ignoring types  same correction  agremenet is  67.89645332246229 %
# Agremeent with location included in other location  ignoring types  same correction  agremenet is  67.89645332246229 %
# Agremeent with exactly same location  ignoring types  ignoring correction  agremenet is  72.95225580376697 %
# Agremeent with location included in other location  ignoring types  ignoring correction  agremenet is  72.95225580376697 %

# Conclusions:
# - the stricter criteria of agreements, the less agreement there is
# - there's only 65% of cases of total inter-annotator agreement with exact match of location type and correction
# - in 2% of corrections for same error and location annotators came up with different corrections
# - in 72% of cases correction of one person would somewhat overlap other person correction, disregarding other error details
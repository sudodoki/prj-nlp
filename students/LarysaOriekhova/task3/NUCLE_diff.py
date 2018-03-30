from os import listdir
import xml.etree.ElementTree as ET

print '\nfiles used: '

def readDataFiles():
    folder_name = '../tmp/nucle'
    all_files = listdir(folder_name)
    files_to_parse = [ filename for filename in all_files if filename.endswith( '.sgml' ) ]

    all_errors = {}
    for f in files_to_parse:
        full_name = folder_name + '/' + f
        print full_name
        xml_doc = ''
        doc_external_tag = 'DOC'
        inside_doc = False
        with open(full_name, 'rb') as error_file:
            xml_content = error_file.read()
            root = ET.fromstring('<root>' + xml_content + "</root>")
            for item in root.findall('DOC'):
                for annotation in item.findall('ANNOTATION'):
                    teacher = annotation.get('teacher_id')
                    if not all_errors.has_key(teacher):
                        all_errors[teacher] = [] 
                    for mistake in annotation.findall('MISTAKE'):
                        type_name = mistake.find('TYPE').text
                        correction = mistake.find('CORRECTION').text
                        if type_name == None:
                            type_name = ''
                        if correction == None:
                            correction = ''
                        
                        error_description = {'start_par': int(mistake.get('start_par')), \
                                             'end_par' : int(mistake.get('end_par')), \
                                             'start_off' : int(mistake.get('start_off')), \
                                             'end_off' : int(mistake.get('end_off')), \
                                             'type' : type_name,
                                             'correction' : correction}
                        all_errors[teacher].append(error_description)
    return all_errors

        
# assume that errors are sorted by positions in text
def calcStats(all_errors):
    
    teacher_ids = all_errors.keys()
    if len(teacher_ids) != 2:
        print 'only 2 teachers can be compared!'
        return

    teach1 = teacher_ids[0]
    teach2 = teacher_ids[1]

    print '\nTeachers:'
    print 'teacher1 id = ', teach1
    print 'teacher2 id = ', teach2
    
    data1 = all_errors[teach1]
    data2 = all_errors[teach2]
    
    common = 0
    skipped_by1 = 0
    skipped_by2 = 0
    range_differs = 0
    type_differs = 0
    correction_diffres = 0
    diff = []
    type_mismatch = {}
    
    ind1 = 0
    ind2 = 0
    while (ind1 < len(data1)) and (ind2 < len(data2)):
        error1 = data1[ind1]
        error2 = data2[ind2]
        diff_description = ''
        
        ### Compare parameters of 2 mistake marks 
        if not type_mismatch.has_key(error1['type']):
            type_mismatch[error1['type']] = {}
        if not type_mismatch[error1['type']].has_key(error2['type']):
            type_mismatch[error1['type']][error2['type']] = 0
        if not type_mismatch.has_key('SkippedError'):
            type_mismatch['SkippedError'] = {}
        if not type_mismatch['SkippedError'].has_key(error2['type']):
            type_mismatch['SkippedError'][error2['type']] = 0
        if not type_mismatch[error1['type']].has_key('SkippedError'):
            type_mismatch[error1['type']]['SkippedError'] = 0
            
        if (error1['start_par'] < error2['start_par']) and \
            (error1['end_par'] < error2['start_par']):
                skipped_by2 += 1
                diff_description += 'SKIPPED_BY_2 '
                ind1 += 1
                type_mismatch[error1['type']]['SkippedError'] += 1

        elif (error2['start_par'] < error1['start_par']) and \
            (error2['end_par'] < error1['start_par']):
                skipped_by1 += 1
                diff_description += 'SKIPPED_BY_1 '
                ind2 += 1
                type_mismatch['SkippedError'][error2['type']] += 1
                
        elif (error1['start_off'] < error2['start_off']) and \
            (error1['end_off'] < error2['start_off']):
                skipped_by2 += 1
                diff_description += 'SKIPPED_BY_2 '
                ind1 += 1
                type_mismatch[error1['type']]['SkippedError'] += 1
                
        elif (error2['start_off'] < error1['start_off']) and \
            (error2['end_off'] < error1['start_off']):
                skipped_by1 += 1
                diff_description += 'SKIPPED_BY_1 '
                ind2 += 1
                type_mismatch['SkippedError'][error2['type']] += 1
                
        elif (error1['start_par'] == error2['start_par']) and \
            (error1['end_par'] == error2['end_par']) and \
            (error1['start_off'] == error2['start_off']) and \
            (error1['end_off'] == error2['end_off']):

                if (error1['type'] !=  error2['type']):
                    type_differs += 1
                    diff_description += 'TYPE '
                    type_mismatch[error1['type']][error2['type']] += 1
                if error1['correction'] !=  error2['correction']:
                    correction_diffres += 1
                    diff_description += 'CORRECTION '

                if (error1['type'] ==  error2['type']) and (error1['correction'] ==  error2['correction']):
                    common += 1

                ind1 += 1
                ind2 += 1
        else:
            range_differs += 1
            diff_description += 'RANGE '
            if (error1['type'] !=  error2['type']):
                type_differs += 1
                diff_description += 'TYPE '
                type_mismatch[error1['type']][error2['type']] += 1
            if error1['correction'] !=  error2['correction']:
                correction_diffres += 1
                diff_description += 'CORRECTION '
            ind1 += 1
            ind2 += 1
        # end compare marks
        
        if len(diff_description) > 0:
            diff.append((error1, error2, diff_description))
        
    print '\nGeneral statistics for annotation agreement: '
    print 'Full match:', common
    print 'Partial match:'
    print '\trange_differs (small differences in start-end interval of mistake):', range_differs
    print '\ttype_differs (ranges are similar, but types are different):', type_differs
    print '\tcorrection_diffres (ranges are similar but correction string are different):', correction_diffres
    print 'Different:'
    print '\tmistakes skipped by teacher 1 (but not skipped by teacher 2):', skipped_by1
    print '\tmistakes skipped by teacher 2 (but not skipped by teacher 1):', skipped_by2
    return diff, type_mismatch      
            
errors_data = readDataFiles()
diff, type_mismatch = calcStats(errors_data)
print '\n\nType mismatch frequencies:'
print 'Type_by_Teacher1', 'Type_By_Teacher2', 'Frequency'
for t1, t2 in type_mismatch.iteritems():
    for t2_name, fr in t2.iteritems():
        if int(fr) > 0:
            print t1, t2_name, str(fr)
        
print '\n\nFull diff for mistakes list '
print 'Format: '
print '{Annotation of teacher 1}'
print '{Annotation of teacher 2}'
print 'WHAT IS DIFFERENT'
for d in diff:
    print d[0]
    print d[1]
    print d[2]

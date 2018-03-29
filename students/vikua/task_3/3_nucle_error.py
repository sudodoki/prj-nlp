import os
import argparse
import shutil

import pandas as pd
from lxml import etree


def is_valid_dir(arg):
    """ Auxiliary function to make sure input file path exists.
    """
    if not os.path.exists(arg) or not os.path.isdir(arg):
        raise argparse.ArgumentTypeError("Path {} doesn't exist or it is not a directory".format(arg))
    return arg


class DataSource(object):
    """ Class to work around an absence of ROOT tag in xml files of NUCLE dataset.
    It intercepts the start and the end of the file and adds <ROOT></ROOT> tags respectively.
    """

    def __init__(self, file_path):
        self.started = False
        self.stoped = False
        self.file_path = file_path

    def read(self, requested_size=0):
        if self.stoped:
            self.file.close()
            return b''

        if not self.started:
            self.started = True
            self.file = open(self.file_path, 'r')
            return b'<ROOT>'
        else:
            try:
                return next(self.file).encode('utf-8')
            except StopIteration:
                self.stoped = True
                return b'</ROOT>'


def xml_to_dataframe(list_of_files):
    """ Parsing hierarchical XML file into flat pandas DataFrame.

    Parameters
    ----------
    list_of_files : list of str
        List of paths to sgml files with XML data

    Returns
    -------
    result : pd.DataFrame
        Flat DataFrame with annotations data
    """
    all_corrections = []

    for file_path in list_of_files:
        ds = DataSource(file_path)

        current_doc_id = None
        current_teacher = None
        current_mistake = dict()
        for event, element in etree.iterparse(ds, events=('start', 'end')):
            if event == 'start':
                if element.tag == 'DOC':
                    current_doc_id = element.attrib['nid']
                elif element.tag == 'ANNOTATION':
                    current_teacher = element.attrib['teacher_id']
                elif element.tag == 'MISTAKE':
                    current_mistake['start_par'] = element.attrib['start_par']
                    current_mistake['end_par'] = element.attrib['end_par']
                    current_mistake['start_off'] = element.attrib['start_off']
                    current_mistake['end_off'] = element.attrib['end_off']
                elif element.tag == 'TYPE':
                    current_mistake['type'] = element.text
                elif element.tag == 'CORRECTION':
                    current_mistake['correction'] = element.text
            elif event == 'end':
                if element.tag == 'MISTAKE':
                    all_corrections.append(
                        (current_doc_id, current_teacher,
                         current_mistake['start_par'], current_mistake['end_par'],
                         current_mistake['start_off'], current_mistake['end_off'],
                         current_mistake['type'], current_mistake['correction'])
                    )
                    current_mistake = dict()

    return pd.DataFrame(all_corrections, columns=['doc_id', 'teacher_id', 'start_par', 'end_par',
                                                  'start_off', 'end_off', 'type', 'correction'])


def get_full_agreement(s):
    """ Calculates inter-annotator agreement for full match of mistakes
    (all attributes like start/end offsets, type, correction etc. should match 100%).

    Calculated as:

        result = len(correction_A intersection correction_B) / len(correction_A union correction_B)

    Parameters
    ----------
    s : list of set
        list of corrections for each teacher

    Returns
    -------
    agreement value (from 0 to 1)
    """
    if len(s) <= 1:
        return 0, 0, 0
    else:
        all_c = set()
        for x in s:
            all_c.update(x)

        intersection = s[0]
        for x in s[1:]:
            intersection = intersection & x
        common_len = len(intersection)
        total_len = len(all_c)
        return total_len, common_len, common_len / float(total_len)


def calculate_general_agreement(df):
    """ Calculates general inter-annotator agreement, which is full agreement
    between teachers across all docs and error types.

    Parameters
    ----------
    df : pd.DataFrame
        Flat DataFrame with annotations data

    Returns
    -------
    result : float
        Value of inter-annotator agreement
    """
    df['hash'] = df.apply(lambda x: '{}:{}:{}:{}:{}:{}:{}'.format(x['doc_id'], x['start_par'],
                                                                  x['end_par'],
                                                                  x['start_off'], x['end_off'],
                                                                  x['type'], x['correction']), axis=1)
    new_df = df.groupby(['teacher_id'])['hash'].agg(lambda x: set(x.tolist()))
    return get_full_agreement(new_df.tolist())


def calculate_per_doc_agreement(df):
    """ Calculates per doc inter-annotator agreement

    Parameters
    ----------
    df : pd.DataFrame
        Flat DataFrame with annotations data

    Returns
    -------
    result : pd.DataFrame
        Keys:
        doc_id - id of the doc
        total_len - total number of unique mistakes for the doc
        common_len - number of common mistakes between teaches
        agreement - inter-annotator agreement
    """
    df['hash'] = df.apply(lambda x: '{}:{}:{}:{}:{}:{}'.format(x['start_par'], x['end_par'],
                                                               x['start_off'], x['end_off'],
                                                               x['type'], x['correction']), axis=1)
    new_df = df.groupby(['doc_id', 'teacher_id'])['doc_id', 'teacher_id', 'hash']\
        .agg(lambda x: set(x.tolist()))
    new_df = new_df.reset_index()
    new_df = new_df.groupby(['doc_id'])['hash'].agg(lambda x: get_full_agreement(x.tolist()))

    result = pd.DataFrame({'doc_id': new_df.index.tolist(), 'agreement': new_df.tolist()})
    result['total_len'] = result['agreement'].apply(lambda x: x[0])
    result['common_len'] = result['agreement'].apply(lambda x: x[1])
    result['agreement'] = result['agreement'].apply(lambda x: x[2])
    return result


def calculate_per_error_agreement(df):
    """ Calculates per error type inter-annotator agreement

    Parameters
    ----------
    df : pd.DataFrame
        Flat DataFrame with annotations data

    Returns
    -------
    result : pd.DataFrame
        Keys:
        error_type - type of error
        total_len - total number of unique mistakes for the doc
        common_len - number of common mistakes between teaches
        agreement - inter-annotator agreement
    """
    df['hash'] = df.apply(
        lambda x: '{}:{}:{}:{}:{}:{}'.format(x['doc_id'], x['start_par'],
                                             x['end_par'], x['start_off'],
                                             x['end_off'], x['correction']), axis=1)
    new_df = df.groupby(['type', 'teacher_id'])['hash'].agg(lambda x: set(x.tolist()))
    new_df = new_df.groupby(['type']).agg(lambda x: get_full_agreement(x.tolist()))

    result = pd.DataFrame({'error_type': new_df.index.tolist(), 'agreement': new_df.tolist()})
    result['total_len'] = result['agreement'].apply(lambda x: x[0])
    result['common_len'] = result['agreement'].apply(lambda x: x[1])
    result['agreement'] = result['agreement'].apply(lambda x: x[2])
    return result


def main(input_dir, output_dir):
    alt_files = [x for x in os.listdir(input_dir) if x.endswith('sgml')]
    alt_files = [os.path.join(input_dir, x) for x in alt_files]

    corrections_df = xml_to_dataframe(alt_files)

    total_len, common_len, agreement = calculate_general_agreement(corrections_df)
    print('General -> total len: {}, common len: {}, agreement: {}'.format(total_len,
                                                                           common_len,
                                                                           agreement))

    per_doc = calculate_per_doc_agreement(corrections_df)
    per_doc.to_csv(os.path.join(output_dir, 'full_per_doc_agreement.csv'), index=False)

    per_error = calculate_per_error_agreement(corrections_df)
    per_error.to_csv(os.path.join(output_dir, 'full_per_error_agreement.csv'), index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='NUCLE Error corpus inter-annotator agreement')
    parser.add_argument('-i', dest='input_dir', type=is_valid_dir,
                        help='Path to directory with annotations')
    parser.add_argument('-o', dest='output_dir', help='Output directory')

    args = parser.parse_args()

    if os.path.exists(args.output_dir):
        shutil.rmtree(args.output_dir)
    os.mkdir(args.output_dir)

    main(args.input_dir, args.output_dir)
import pandas as pd

input_file1 = 'official-2014.0.sgml'
#output_file1 = 'official-2014.0.sgml'

input_file2 = 'official-2014.1.sgml'
#output_file2 = 'official-2014.1.sgml'


import xml.sax

class Nid():
    def __init__(self):
        self.nid = []
        self.teacher_id = []
        self.start_par = []
        self.start_off = []
        self.end_par = []
        self.end_off = []
        self.type = []
        self.CORRECTION =[]



class XMLHandler(xml.sax.ContentHandler):
    def __init__(self, nd):
        self.CurrentData = ""
        self.id = ""
        self.MISTAKE = ""
        self.start_par = ""
        self.start_off = ""
        self.end_par = ""
        self.end_off = ""
        self.type = ""
        self.CORRECTION = ""


        self.nid = 0
        self.nd = nd

    def startElement(self, tag, attributes):
        self.CurrentData = tag
        if tag == "ANNOTATION":
            self.id = attributes["teacher_id"]

            print("ANNOTATION:", self.id)
        elif tag == "MISTAKE":
            start_par = attributes["start_par"]
            start_off = attributes["start_off"]
            end_par = attributes["end_par"]
            end_off = attributes["end_off"]
            print("MISTAKE: {}, {}, {}, {}".format(start_par,start_off,end_par,end_off))
            self.nd.teacher_id.append(self.id)
            #self.teacher_id.append(self.id)
            self.nd.start_par.append(start_par)
            self.nd.start_off.append(start_off)
            self.nd.end_par.append(end_par)
            self.nd.end_off.append(end_off)
            self.nd.nid.append(self.nid)
        elif tag == "DOC":
            self.nid = attributes["nid"]
            print("nid = {}".format(self.nid))


    def endElement(self, tag):
        if self.CurrentData == "ANNOTATION":
            print("Type:", self.ANNOTATION)
        self.CurrentData = ""


    def characters(self, content):
        if self.CurrentData == "TYPE":
            type = content
            print("TYPE:", type)
            self.nd.type.append(content)
        elif self.CurrentData == "CORRECTION":
            correction = content
            self.nd.CORRECTION.append(content)
            print("CORRECTION:", correction)

    def fatalError(self, msg):
        print(msg)

def get_data_to_df(file, parser):
    file1 = Nid()


    Handler = XMLHandler(file1)
    parser.setContentHandler(Handler)

    parser.parse(file)

    df1 = pd.DataFrame(
        columns=['NID', 'CORRECTION', 'TYPE', 'start_par', 'start_off', 'end_par', 'end_off'])#, 'teacher_id'])
    se = pd.Series(file1.teacher_id)
    #df1['teacher_id'] = se.values
    se = pd.Series(file1.nid)
    df1['NID'] = se.values

    se = pd.Series(file1.CORRECTION)
    df1['CORRECTION'] = se.values

    se = pd.Series(file1.type)
    df1['TYPE'] = se.values

    se = pd.Series(file1.start_par)
    df1['start_par'] = se.values

    se = pd.Series(file1.start_off)
    df1['start_off'] = se.values

    se = pd.Series(file1.end_par)
    df1['end_par'] = se.values

    se = pd.Series(file1.end_off)
    df1['end_off'] = se.values

    return df1


def get_different_rows(source_df, new_df):
    merged_df = source_df.merge(new_df, indicator=True, how='outer')
    changed_rows_df = merged_df[merged_df['_merge'] == 'right_only']
    return changed_rows_df.drop('_merge', axis=1)


if (__name__ == "__main__"):
    file1 = Nid()
    file2 = Nid()
    parser = xml.sax.make_parser()
    parser.setFeature(xml.sax.handler.feature_namespaces, 0)

    df1 = get_data_to_df(input_file1, parser)

    df2 = get_data_to_df(input_file2, parser)


    total_corrections = df2.size #take as total corrections second teacher, as he had more corrections
    print(total_corrections)
    #'NID', 'CORRECTION', 'TYPE', 'start_par', 'start_off', 'end_par', 'end_off'

    df3 = df2.merge(df1, left_on=df2.columns.tolist(), right_on=df1.columns.tolist(), how='inner')
    all = (df3.size / total_corrections) * 100
    print(df3.size)
    print(all) #13.3293305314


    #without type
    df1.drop('TYPE', axis=1, inplace=True)
    df2.drop('TYPE', axis=1, inplace=True)
    df3 = df2.merge(df1, left_on=df2.columns.tolist(), right_on=df1.columns.tolist(), how='inner')
    all = (df3.size / total_corrections) * 100
    print(all) #13.9726379894


    #without correction and type
    df1.drop('CORRECTION',axis=1,inplace=True)
    df2.drop('CORRECTION', axis=1, inplace=True)
    df3 = df2.merge(df1, left_on=df2.columns.tolist(), right_on=df1.columns.tolist(), how='inner')
    all = (df3.size / total_corrections) * 100
    print(all) #24.9174422095

#RESULT:
#using all parameters: 13.32%
#without type: 13.97%
#without type and correction: 24.91%
#without correction but with type: 20.92%

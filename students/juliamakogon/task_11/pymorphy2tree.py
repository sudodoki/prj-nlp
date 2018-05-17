from collections import OrderedDict
import pymorphy2
import tokenize_uk

class Pymorhy2Tree():
    '''
    The class to get pymorhy2 information for the UD tree
    '''
    def __init__(self, lang='uk'):
        self.morph = pymorphy2.MorphAnalyzer(lang=lang)

    # p.tag.mood, p.tag.transitivity, p.tag.voice, p.tag.involvement
    mapping_pos = {"ADJF": "ADJ", "ADJS": "ADJ", "COMP": "ADJ", "PRTF": "ADJ",
           "PRTS": "ADJ", "GRND": "VERB", "NUMR": "NUM", "ADVB": "ADV",
           "NPRO": "PRON", "PRED": "ADV", "PREP": "ADP",
           "PRCL": "PART"}
    mapping_tag = {"PNCT": "PUNCT", "UNKN":"X", "LATN":"X", "NUMB":"NUM", "ROMN":"NUM", "intg":"NUM", "real":"NUM"}
    mapping_gender = {'masc':'Masc', 'femn':'Fem', 'neut':'Neut'}
    mapping_animacy = {'anim':'Anim', 'inan':'Inan'}
    mapping_number = {'sing':'Sing', 'plur':'Plur'}
    mapping_case = {'nomn':'Nom', 'gent':'Gen', 'datv':'Dat', 'accs':'Acc', 'ablt':'Abl', 'loct':'Loc', 'voct':'Voc', 
                    'gen2':'Gen', 'acc2':'Acc', 'loc2':'Loc'}
    mapping_aspect = {'perf':'Perf', 'impf':'Imp'}
    mapping_voice = {'actv':'Act', 'pssv':'Pass'} 
    mapping_person = {'1per':'1', '2per':'2', '3per':'3'}
    mapping_tense = {'pres':'Pres', 'past':'Past', 'futr':'Fut'}

    @staticmethod
    def normalize_pos(word):
        '''
        Converts Pymorphy2 POS tags to UD POS
        '''
        pos = 'X'
        if not word.tag.POS:
            for x in Pymorhy2Tree.mapping_tag:
                if x in word.tag:
                    return Pymorhy2Tree.mapping_tag[x]
        else:
            if word.tag.POS == "CONJ":
                if "coord" in word.tag:
                    return "CCONJ"
                else:
                    return "SCONJ"
            else:
                return Pymorhy2Tree.mapping_pos.get(word.tag.POS, str(word.tag.POS))
        return pos

    @staticmethod
    def normalize_feats(word):
        '''
        Gets features from Pymorphy2 OpenCorpora info (except POS) and converts to UD-formatted OrderedDict
        '''
        feats = OrderedDict()
        if 'LATN' in word.tag:
            feats['Foreign'] = 'Yes'
        if word.tag.gender:
            feats['Gender'] = Pymorhy2Tree.mapping_gender.get(word.tag.gender, str(word.tag.gender))
    #     else:
    #         try:
    #             #http://pymorphy2.readthedocs.io/en/latest/user/grammemes.html#russian-genders
    #             if 'ms-f' in word.tag:
    #                 feats['Gender'] = 'Com'
    #         except:
    #             logging.exception("checking 'Ms-f' in word.tag")
        if word.tag.animacy:
            feats['Animacy'] = Pymorhy2Tree.mapping_animacy.get(word.tag.animacy, str(word.tag.animacy))
        if word.tag.number:
            feats['Number'] = Pymorhy2Tree.mapping_number.get(word.tag.number, str(word.tag.number)) 
        if word.tag.case:
            feats['Case'] = Pymorhy2Tree.mapping_case.get(word.tag.case, str(word.tag.case))
        if word.tag.aspect:
            feats['Aspect'] = Pymorhy2Tree.mapping_aspect.get(word.tag.aspect, str(word.tag.aspect)) 
        if word.tag.voice:
            feats['Voice'] = Pymorhy2Tree.mapping_voice.get(word.tag.voice, str(word.tag.voice))
        if word.tag.person:
            feats['Person'] = Pymorhy2Tree.mapping_person.get(word.tag.person, str(word.tag.person) )
        if word.tag.tense:
            feats['Tense'] = Pymorhy2Tree.mapping_tense.get(word.tag.tense, str(word.tag.tense))
        return feats     

    def parse_tree(self, tokens):
        '''
        Forms connlu tree from tokens, for each token gets id, form, lemma, upostag, feats
        '''
        tree = []
        for i, t in enumerate(tokens, 1):
            node = OrderedDict()
            node['id'] = i
            node['form'] = t
            parsed = self.morph.parse(t)
            p = parsed[0] if parsed else None
            node['lemma'] = p.normal_form if p else t
            node['upostag'] = Pymorhy2Tree.normalize_pos(p) if p else None
            feats = Pymorhy2Tree.normalize_feats(p)
            node['feats'] = feats if len(feats) > 0 else None
            node['head'] = None
            node['deprel'] = None
            tree.append(node)
        return tree 

    def parse_tree_connlu(self, tree):
        '''
        Gets Pymorphy2 info for nodes of connlu tree, see parse_tree method
        '''
        tokens = [node['form'] for node in tree]
        return self.parse_tree(tokens)      

    def parse_sentence(self, sentence):
        '''
        Tokenizes the sentence to words using tokenize_uk, converts to connlu and gets Pymorphy2 info, see parse_tree method
        '''
        tokens = tokenize_uk.tokenize_words(sentence)
        return self.parse_tree(tokens)    


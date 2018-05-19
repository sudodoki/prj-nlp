import logging, re, pymorphy2, pdb
from polyglot.text import Text
from polyglot.detect import Detector
from operator import itemgetter

morph_ru = pymorphy2.MorphAnalyzer(lang='ru')
morph_uk = pymorphy2.MorphAnalyzer(lang='uk')

def lemmatize_word(w, lang):
    '''
    Lemmatizes a word and returns its lemma__POS-tag
    '''
    morph = morph_uk if lang == 'uk' else morph_ru
    parsed = morph.parse(w)
    if len(parsed) < 1 or any(
        isinstance(method[0], pymorphy2.units.unkn.UnknAnalyzer)
        for method in parsed[0].methods_stack):
        return w

    parsed = parsed[0]
    ner_tag = re.search('Geox|Name|Surn|Orgn|Abbr|Patr|Trad', str(parsed.tag))
    try:
        return f'{parsed.normal_form}__{ner_tag.group()}' \
               if ner_tag \
               else f'{parsed.normal_form}__{parsed.tag.POS if parsed.tag.POS else re.search("[A-Z][A-Za-z]{3,6}", str(parsed.tag)).group()}'
    except Exception as e:
        pdb.set_trace()
        raise e

def get_lang(text):
    '''
    Detects whether text is in Russian or in Ukrainian.
    Accepts string or list of words as input
    Returns None if unsure. 
    '''
    if isinstance(text, list) and all(isinstance(w, str) for w in text):
        text = ' '.join(text)
    elif isinstance(text, str):
        pass
    else:
        raise TypeError(f'{type(text)} provided as input. Please give a str or a list of words')
        
    lang = [{'lang': l.code, 'sure': l.confidence}
            for l in Detector(text, quiet=True).languages
            if l.code in ['uk', 'ru']]
    lang = sorted(lang, key=itemgetter('sure'))
    if len(lang) > 0:
        return lang[-1]['lang']
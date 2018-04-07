# here we apply our rules to retrieve albums from wiki page

import re
import regex
import spacy
nlp = spacy.load('en_core_web_lg')
from nltk.tokenize import sent_tokenize

def clean_sent(sent):
    res = re.sub('\n', '', sent)
    res = re.sub('\[[0-9]+\]', '', sent)
    return res.strip()

# first we define our baseline rule
def baseline_rule(sent):
    """
    Rule that looks for pattern of the type
    'album <Title>' if a sentence has year in it.
    """
    yearpat = r'19[7-9][0-9]|20[0-1][0-9]'
    albumpat = r'album((\s|,)\s?[A-Z].+?(\.|,|\())'
    if ('album' in sent) and re.search(yearpat, sent):
        albumname = re.search(albumpat, sent)
        if not albumname:
            return None
        title, year = albumname.group(1).strip(' ,.!()'), int(re.findall(yearpat, sent)[0])
        return [(title, year)]
    return None

def baseline(text):
    """
    A baseline function for the rule-based album extraction:
    it takes a wiki text and looks for pattern of the type
    'album <Title>' if a sentence has year in it.
    """
    albums = []
    sentences = [clean_sent(s) for s in text.split('.')]
    yearpat = r'19[7-9][0-9]|20[0-1][0-9]'
    albumpat = r'album((\s|,)\s?[A-Z].+?(\.|,|\())'
    for sent in sentences:
        if baseline_rule(sent):
            albums.extend(baseline_rule(sent))
    return set(albums)

# now functions needed for our full rule-based system
def extract_title(fragment):
    """
    A function that uses spacy POS-tagger to find out
    if a fragment of text contains title (of album)
    """
    doc = nlp(fragment, disable=['ner', 'parser'])
    title = ''
    lower_pos = ['ADP', 'SCONJ', 'CCONJ', 'DET']
    temp_str = ''
    temp_count = 0
    for token in doc:
        if temp_count > 1:
            break
        if token.is_title and (temp_str != ''):
            title += (temp_str + token.text_with_ws)
            temp_str = ''
        elif token.is_title:
            title += token.text_with_ws
        elif token.pos_ in lower_pos:
            if title == '':
                break
            temp_str += token.text_with_ws
            temp_count += 1
        elif token.text == '(':
            title += token.text_with_ws
        elif token.is_punct:
            temp_str += token.text_with_ws
        elif token.is_digit and title.endswith('('):
            title = title[:-1]
            break
        elif token.is_digit and ('in' in temp_str):
            break
        elif token.is_digit:
            title += (temp_str + token.text_with_ws)
            temp_str = ''
        elif token.is_lower and (token.text == "n't"):
            title += token.text_with_ws
        elif token.is_lower:
            break
        elif temp_str != '':
            temp_str = ''
    if ('(' in title) and not (')' in title):
        title = title.split('(')[0]
    date_pat = r'.*?in (?:January|February|March|April|May|June|July|August|September|October|November|December)'
    date_pat2 = r'.*?on [0-9]+ (?:January|February|March|April|May|June|July|August|September|October|November|December)'
    year_pat = r'\s?19[5-9][0-9]|20[0-1][0-9].*'
    if re.search(date_pat, title):
        title = title.split('in ')[0]
    elif re.search(date_pat2, title):
        title = title.split('on ')[0]
    return title.strip(' ,:')

# now actually rules
def rule_0(sent):
    """
    Restatement of the baseline rule
    that looks for pattern of the type
    'album <Title>' if a sentence has year in it.
    """
    yearpat = r'19[5-9][0-9]|20[0-1][0-9]'
    if ('album' in sent) and re.search(yearpat, sent):
        year = int(re.findall(yearpat, sent)[0])
        title_ind = sent.find('album')+6
        fragment = sent[title_ind:]
        title = extract_title(fragment)
        return [(title, year)]
    return None

def rule_1(sent):
    """
    This rule is quite unambigous: it captures the phrase
    "In [year], [band] released [album]."
    """
    rule_pat = r'[Ii]n (19[5-9][0-9]|20[0-1][0-9]),?.*?(?:released|recorded)\s?([A-Z].+?)(\.|,|\()'
    match = regex.search(rule_pat, sent)
    if not match:
        return None
    year, title = int(match.captures(1)[0]), match.captures(2)[0].strip()
    return [(title, year)]

def rule_2(sent):
    """
    Looking for pattern:
    ...albums|releases: [title] ([year]), ...
    """
    albums = []
    rule_pat = r'.*?(?:albums|releases)[:,](.*?)(?:[.?!;]|$)'
    album_pat = r'(.*?)\((19[5-9][0-9]|20[0-1][0-9]\)?)'
    if ('albums' in sent) or ('recordings:' in sent):
        album_match = re.search(rule_pat, sent)
        if not album_match:
            return None
        album_list = re.split(r'\),\s?|\sand', album_match.group(1))
        for a in album_list:
            match = regex.search(album_pat, a)
            if not match:
                continue
            title, year = match.captures(1)[0].strip(), int(match.captures(2)[0].strip(' ,()'))
            albums.append((title, year))
    return albums

def rule_3(sent):
    """
    For pattern of a form:
    ... [year]'s [title], ...
    """
    rule_pat = r"^(?:.*?)?(19[5-9][0-9]|20[0-1][0-9])\'s\s(.*)"
    match = regex.search(rule_pat, sent)
    if not match:
        return None
    year = int(match.captures(1)[0])
    title = extract_title(match.captures(2)[0])
    return [(title, year)]

def rule_4(sent):
    """
    For pattern of a form:
    [year] saw the release of [title]...
    """
    rule_pat = r"^(?:.*?)?(19[5-9][0-9]|20[0-1][0-9]) saw the release of (.*?)(?:[;:.,(-]|$)"
    match = regex.search(rule_pat, sent)
    if not match:
        return None
    year, title = int(match.captures(1)[0]), match.captures(2)[0].strip()
    return [(title, year)]

def rule_5(sent):
    """
    For pattern of a form:
    ... album ([title, year])
    """
    rule_pat = r'.*?album \((.*?)(19[5-9][0-9]|20[0-1][0-9])\)'
    match = regex.search(rule_pat, sent)
    if not match:
        return None
    title, year = match.captures(1)[0].strip(' ,'), int(match.captures(2)[0])
    return [(title, year)]

def rule_6(sent):
    """
    For pattern of a form:
    [year] release|album, [title]
    """
    rule_pat = r'.*?(19[5-9][0-9]|20[0-1][0-9])\s(?:release|album),(.*)'
    match = regex.search(rule_pat, sent)
    if not match:
        return None
    year = int(match.captures(1)[0])
    title = extract_title(match.captures(2)[0])
    return [(title, year)]

def rule_7(sent):
    """
    Like rule_0, but looking
    for 'debut' or 'release' or 'LP'
    """
    yearpat = r'19[5-9][0-9]|20[0-1][0-9]'
    words = ['debut', 'release', 'LP', 'debut recording']
    for w in words:
        if w in sent and re.search(yearpat, sent):
            year = int(re.findall(yearpat, sent)[0])
            title_ind = sent.find(w) + len(w) + 1
            fragment = sent[title_ind:]
            title = extract_title(fragment)
            if len(title) > 2:
                return [(title, year)]
    return None

def rule_8(sent):
    """
    Similar to rule 3,
    for pattern of a form:
    ... [year]'s [title], [year]'s [title],...
    """
    rule_pat = r"(?:19[5-9][0-9]|20[0-1][0-9])\'s\s.*?(?:,|and|$)"
    matched_albums = regex.findall(rule_pat, sent)
    if not matched_albums:
        return None
    albums = []
    for ma in matched_albums:
        albums.append(rule_3(ma)[0])
    return albums

def rule_9(sent):
    """
    For pattern of a form:
    (in) [year], ... released [title]
    """
    rule_pat = r'.*?(19[5-9][0-9]|20[0-1][0-9]).*?(?:released|recorded)\s(?:album)?(.*)'
    match = regex.search(rule_pat, sent)
    if not match: return None
    year = int(match.captures(1)[0])
    title = extract_title(match.captures(2)[0])
    return [(title, year)]

def rule_10(sent):
    """
    For pattern of a form:
    released in [year], [title]
    """
    rule_pat = r"[Rr]eleased in (19[5-9][0-9]|20[0-1][0-9]), (.*)"
    match = regex.search(rule_pat, sent)
    if not match: return None
    year = int(match.captures(1)[0])
    title = extract_title(match.captures(2)[0])
    return [(title, year)]

def rule_11(sent):
    """
    For pattern of a form:
    [title] was released ... [year]
    """
    rule_pat = r"(.*?) was released.*?(19[5-9][0-9]|20[0-1][0-9])"
    match = regex.search(rule_pat, sent)
    if not match: return None
    year = int(match.captures(2)[0])
    fragment = match.captures(1)[0]
    title = extract_title(match.captures(1)[0])
    if (title == 'The') or (title == 'A'):
        return None
    return [(title, year)]

def rule_12(sent):
    """
    For a pattern of a form:
    In [year], [album] was released
    """
    rule_pat = r'In .*?(19[5-9][0-9]|20[0-1][0-9]),(.*?)was released'
    match = regex.search(rule_pat, sent)
    if not match: return None
    year = int(match.captures(1)[0])
    fragment = match.captures(2)[0]
    title = extract_title(fragment)
    return [(title, year)]


# finally a function to combine all the rules
def apply_rules(text):
    """
    Use each rule from the list of rules
    to the text, and return a set of found albums.
    """
    rules = [rule_0, rule_1, rule_2, rule_3, rule_4, rule_5,
             rule_6, rule_7, rule_8, rule_9, rule_10,
             rule_11, rule_12]
    albums = []
    sentences = [clean_sent(s) for s in sent_tokenize(text)]
    for sent in sentences:
        for rule in rules:
            if rule(sent):
                albums.extend(rule(sent))
    albums = [(title.strip(' :,;."'), year) for (title, year) in albums if title != '']
    albums = [(title, year) for (title, year) in albums if not title[0].islower()]
    return albums
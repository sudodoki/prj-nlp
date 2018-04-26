NULL_TUPLE = ('NULL', 'NULL')

WORD = 0
POS = 1
HEAD = 2
LABEL = 3
LEMMA = 4
FEATS = 5

COMMON_FEATS = {'Case',
     'Number',
     'Gender',
     'Animacy',
     'Aspect',
     'VerbForm',
     'Mood',
     'PronType',
     'Tense',
     'Person',
     'Degree',
     'PunctType',
     'Uninflect',
     'NumType',
     'NameType'}

def null_feats(word_label):
    """
    Return NULL for all common feats.
    """
    features = {}
    for f in COMMON_FEATS:
        features[word_label+"_"+f+"=NULL"] = 1
    return features

def get_feats(sent, word_idx, word_label):
    """
    Get morphological feats of the word.
    word_lebel is either s0 or b0.
    """
    word = sent[word_idx]
    if not sent[word_idx][FEATS]:
        return {word_label+"_feats=NULL": 1}
    features = {word_label+"_feats=NOTNULL": 1}
    for k in word[FEATS].keys():
        feat_name = "{wl}_{featk}={featv}".format(
                    wl=word_label, featk=k, 
                    featv=word[FEATS][k])
        features.update({feat_name: 1})
    return features

def get_feats2(sent, word_idx, word_label):
    """
    Second implementation for morph features.
    Uses template more in line with other features.
    """
    features = {}
    word = sent[word_idx]
    if not word[FEATS]:
        features.update(null_feats(word_label))
    else:
        for k in word[FEATS].keys():
            feat_label = "{wl}_{k}={v}".format(
                wl=word_label, k=k, v=word[FEATS][k])
            features[feat_label] = 1
        for f in COMMON_FEATS-set(word[FEATS].keys()):
            features[word_label+"_"+f+"=NULL"] = 1
    return features
    

def head_children(arcs, h, sent):
    children = list(filter(lambda x: x[0] == h, arcs))
    if len(children):
        lc1 = min(d for (h, d) in children)
        rc1 = max(d for (h, d) in children)
        return sent[lc1], sent[rc1]
    return NULL_TUPLE, NULL_TUPLE

def baseline(conf):
    sentence = conf.sentence
    fv = {}

    b0w = "NULL"
    b0p = "NULL"
    b10p = "NULL"
    b1w = "NULL"
    b1p = "NULL"
    b2w = "NULL"
    b2p = "NULL"
    s0w = "NULL"
    s0p = "NULL"
    s10p = "NULL"
    sr0p = "NULL"
    sh0p = "NULL"
    heads = dict((arc[1], arc[0]) for arc in conf.arcs)

    if len(conf.buffer) > 0:
        b0_pos = conf.buffer[0]
        if b0_pos < len(sentence):
            b0w = sentence[b0_pos][WORD]
            b0p = sentence[b0_pos][POS]
        else:
            b0w = "ROOT"
            b0p = "ROOT"
        hc = head_children(conf.arcs, b0_pos, sentence)
        left_most = hc[0]
        b10p = left_most[POS]
       
        if len(conf.buffer) > 1:
            b1_pos = conf.buffer[1]
            if b1_pos < len(sentence):
                b1w = sentence[b1_pos][WORD]
                b1p = sentence[b1_pos][POS]
            else:
                b1w = "ROOT"
                b1p = "ROOT"
            if len(conf.buffer) > 2:
                b2_pos = conf.buffer[2]
                if b2_pos < len(sentence):
                    b2w = sentence[b2_pos][WORD]
                    b2p = sentence[b2_pos][POS]
                else:
                    b2w = "ROOT"
                    b2p = "ROOT"

    if len(conf.stack) > 0:
        s0_pos = conf.stack[-1]
        if s0_pos < len(sentence):
            s0w = sentence[s0_pos][WORD]
            s0p = sentence[s0_pos][POS]
            
        else:
            s0w = "ROOT"
            s0p = "ROOT"
        hc = head_children(conf.arcs, s0_pos, sentence)
        left_most = hc[0]
        s10p = left_most[POS]
        right_most = hc[1]
        sr0p = right_most[POS]

        sh0p = "NULL"
        if s0_pos in heads:
            sh0p = sentence[heads[s0_pos]][POS]

    b0wp = b0w + "/" + b0p
    b1wp = b1w + "/" + b1p
    s0wp = s0w + "/" + s0p
    b2wp = b2w + "/" + b2p

    fv["s0wp=" + s0wp] = 1
    fv["s0w=" + s0w] = 1
    fv["s0p=" + s0p] = 1
    fv["b0wp=" + b0wp] = 1
    fv["b0w=" + b0w] = 1
    fv["b0p=" + b0p] = 1
    fv["b1wp=" + b1wp] = 1
    fv["b1w=" + b1w] = 1
    fv["b1p=" + b1p] = 1
    fv["b2wp=" + b2wp] = 1
    fv["b2w=" + b2w] = 1
    fv["b2p=" + b2p] = 1

    s0wp_b0wp = s0wp + ";" + b0wp
    s0wp_b0w = s0wp + ";" + b0w
    s0w_b0wp = s0w + ";" + b0wp
    s0wp_b0p = s0wp + ";" + b0p
    s0p_b0wp = s0p + ";" + b0wp
    s0w_b0w = s0w + ";" + b0w
    s0p_b0p = s0p + ";" + b0p
    b0p_b1p = b0p + ";" + b1p

    fv["s0wp_b0wp=" + s0wp_b0wp] = 1
    fv["s0wp_b0w=" + s0wp_b0w] = 1
    fv["s0w_b0wp=" + s0w_b0wp] = 1
    fv["s0wp_b0p=" + s0wp_b0p] = 1
    fv["s0p_b0wp=" + s0p_b0wp] = 1
    fv["s0w_b0w=" + s0w_b0w] = 1
    fv["s0p_b0p=" + s0p_b0p] = 1
    fv["b0p_b1p" + b0p_b1p] = 1

    b0p_b1p_b2p = b0p + ";" + b1p + ";" + b2p
    s0p_b0p_b1p = s0p + ";" + b0p + ";" + b1p
    sh0p_s0p_b0p = sh0p + ";" + s0p + ";" + b0p
    s0p_s10p_b0p = s0p + ";" + s10p + ";" + b0p
    s0p_sr0p_b0p = s0p + ";" + sr0p + ";" + b0p
    s0p_b0p_b10p = s0p + ";" + b0p + ";" + b10p
    fv["b0p_b1p_b2p=" + b0p_b1p_b2p] = 1
    fv["s0p_b0p_b1p=" + s0p_b0p_b1p] = 1
    fv["sh0p_s0p_b0p=" + sh0p_s0p_b0p] = 1
    fv["s0p_s10p_b0p=" + s0p_s10p_b0p] = 1
    fv["s0p_sr0p_b0p=" + s0p_sr0p_b0p] = 1
    fv["s0p_b0p_b10p" + s0p_b0p_b10p] = 1
    
    return fv

# This was intended to be a lot more impressive than the baseline, but so far its only adding 2 features
# soon, though!
def ex(conf):
    """
    Features we use currently:
    buffer[0] - word form, POS
    buffer[0] leftmost child - POS
    buffer[0] righmost child - POS
    buffer[1] - word form, POS
    buffer[2] - word form, POS
    stack[0] - word form, POS
    stack[0] leftmost child - POS
    stack[0] righmost child - POS
    stack[0] head - POS
    stack[1] - word form, POS
    
    Also unigrams, bigrams, trigrams.
    
    Now added:
    lemma for stack[0], buffer[0]
    morph features for stack[0], buffer[0]
    distance between stack[0], buffer[0]
    """
    
    sentence = conf.sentence
    fv = {}

    b0w = "NULL"
    b0p = "NULL"
    b0l = "NULL"
    b0leftcp = "NULL"
    b0rightcp = "NULL"
    b1w = "NULL"
    b1p = "NULL"
    b2w = "NULL"
    b2p = "NULL"
    
    s0w = "NULL"
    s0p = "NULL"
    s0l = "NULL"
    s1w = "NULL"
    s1p = "NULL"
    s0leftcp = "NULL"
    s0rightcp = "NULL"
    sh0p = "NULL"
    
    s0_b0_distance = 1
    s0feats = {}
    b0feats = {}
    
    heads = dict((arc[1], arc[0]) for arc in conf.arcs)

    if len(conf.buffer) > 0:
        b0_pos = conf.buffer[0]
        if b0_pos < len(sentence):
            b0w = sentence[b0_pos][WORD]
            b0p = sentence[b0_pos][POS]
            b0l = sentence[b0_pos][LEMMA]
            b0feats = get_feats(sentence, b0_pos, 'b0')
        else:
            b0w = "ROOT"
            b0p = "ROOT"
            b0l = "ROOT"
        hc = head_children(conf.arcs, b0_pos, sentence)
        left_most = hc[0]
        right_most = hc[1]
        b0leftcp = left_most[POS]
        b0rightcp = right_most[POS]
        
        
        if len(conf.buffer) > 1:
            b1_pos = conf.buffer[1]
            if b1_pos < len(sentence):
                b1w = sentence[b1_pos][WORD]
                b1p = sentence[b1_pos][POS]
            else:
                b1w = "ROOT"
                b1p = "ROOT"
            if len(conf.buffer) > 2:
                b2_pos = conf.buffer[2]
                if b2_pos < len(sentence):
                    b2w = sentence[b2_pos][WORD]
                    b2p = sentence[b2_pos][POS]
                else:
                    b2w = "ROOT"
                    b2p = "ROOT"

    if len(conf.stack) > 0:
        s0_pos = conf.stack[-1]
        if s0_pos < len(sentence):
            s0w = sentence[s0_pos][WORD]
            s0p = sentence[s0_pos][POS]
            s0l = sentence[s0_pos][LEMMA]
            s0feats = get_feats(sentence, s0_pos, 's0')
        else:
            s0w = "ROOT"
            s0p = "ROOT"
            s0l = "ROOT"

        if len(conf.stack) > 1:
            s1_pos = conf.stack[-2]
            s1w = sentence[s1_pos][WORD]
            s1p = sentence[s1_pos][POS]

        hc = head_children(conf.arcs, s0_pos, sentence)
        left_most = hc[0]
        s0leftcp = left_most[POS]
        right_most = hc[1]
        s0rightcp = right_most[POS]
        
        sh0p = "NULL"
        if s0_pos in heads:
            sh0p = sentence[heads[s0_pos]][POS]
            
    if len(conf.buffer) > 0 and len(conf.stack) > 0:
        s0_pos = conf.stack[-1]
        b0_pos = conf.buffer[0]
        if s0_pos == len(sentence):
            s0_b0_distance = b0_pos
        elif b0_pos == len(sentence):
            s0_b0_distance = s0_pos
        else:
            s0_b0_distance = abs(s0_pos-b0_pos)

    b0wp = b0w + "/" + b0p
    b1wp = b1w + "/" + b1p
    s0wp = s0w + "/" + s0p
    s1wp = s1w + "/" + s1p
    b2wp = b2w + "/" + b2p

    fv["s0wp=" + s0wp] = 1
    fv["s0w=" + s0w] = 1
    fv["s0p=" + s0p] = 1
    fv["s0l=" + s0l] = 1
    fv["s1wp=" + s1wp] = 1
    fv["s1w=" + s1w] = 1
    fv["s1p=" + s1p] = 1

    fv["b0wp=" + b0wp] = 1
    fv["b0w=" + b0w] = 1
    fv["b0p=" + b0p] = 1
    fv["b0l=" + b0l] = 1
    fv["b1wp=" + b1wp] = 1
    fv["b1w=" + b1w] = 1
    fv["b1p=" + b1p] = 1
    fv["b2wp=" + b2wp] = 1
    fv["b2w=" + b2w] = 1
    fv["b2p=" + b2p] = 1
    
    fv["b0leftcp=" + b0leftcp] = 1
    fv["b0rightcp=" + b0rightcp] = 1
    fv["s0leftcp=" + s0leftcp] = 1
    fv["s0rightcp=" + s0rightcp] = 1
    fv["s0_b0_distance"] = s0_b0_distance

    s0wp_b0wp = s0wp + ";" + b0wp
    s0wp_b0w = s0wp + ";" + b0w
    s0w_b0wp = s0w + ";" + b0wp
    s0wp_b0p = s0wp + ";" + b0p
    s0p_b0wp = s0p + ";" + b0wp
    s0w_b0w = s0w + ";" + b0w
    s0p_b0p = s0p + ";" + b0p
    b0p_b1p = b0p + ";" + b1p

    fv["s0wp_b0wp=" + s0wp_b0wp] = 1
    fv["s0wp_b0w=" + s0wp_b0w] = 1
    fv["s0w_b0wp=" + s0w_b0wp] = 1
    fv["s0wp_b0p=" + s0wp_b0p] = 1
    fv["s0p_b0wp=" + s0p_b0wp] = 1
    fv["s0w_b0w=" + s0w_b0w] = 1
    fv["s0p_b0p=" + s0p_b0p] = 1
    fv["b0p_b1p=" + b0p_b1p] = 1

    b0p_b1p_b2p = b0p + ";" + b1p + ";" + b2p
    s0p_b0p_b1p = s0p + ";" + b0p + ";" + b1p
    sh0p_s0p_b0p = sh0p + ";" + s0p + ";" + b0p
    s0p_s0leftcp_b0p = s0p + ";" + s0leftcp + ";" + b0p
    s0p_s0rightcp_b0p = s0p + ";" + s0rightcp + ";" + b0p
    s0p_b0p_b0leftcp = s0p + ";" + b0p + ";" + b0leftcp
    s0p_b0p_b0rightcp = s0p + ";" + b0p + ";" + b0rightcp
    fv["b0p_b1p_b2p=" + b0p_b1p_b2p] = 1
    fv["s0p_b0p_b1p=" + s0p_b0p_b1p] = 1
    fv["sh0p_s0p_b0p=" + sh0p_s0p_b0p] = 1
    fv["s0p_s0leftcp_b0p=" + s0p_s0leftcp_b0p] = 1
    fv["s0p_s0rightcp_b0p=" + s0p_s0rightcp_b0p] = 1
    fv["s0p_b0p_b0leftcp=" + s0p_b0p_b0leftcp] = 1
    fv["s0p_b0p_b0rightcp=" + s0p_b0p_b0rightcp] = 1
    
    fv.update(b0feats)
    fv.update(s0feats)

    return fv

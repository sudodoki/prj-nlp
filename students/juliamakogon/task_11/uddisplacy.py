from spacy import displacy
from IPython.core.display import display

def ud_to_displacy(tree, compact=False, jupyter=True, show_deprel = True):
    words = []
    arcs = []
    for node in tree:
        words.append({'text':node['form'], 'tag':node['upostag']})
        head = node['head'] if 'head' in node else None
        deprel = node['deprel'] if 'deprel' in node else None
        if head != None and head > 0:
            is_left = node['id'] < head
            arc_item = {'start':(node['id']-1 if is_left else head-1), 'end':(head-1 if is_left else node['id']-1), 'dir':('left' if is_left else 'right')}
            arc_item['label'] = deprel if deprel and show_deprel else ''
            arcs.append(arc_item)
    ex = { 'words':words, 'arcs':arcs }
    options = { 'compact': compact }
    display(displacy.render(ex, style='dep', manual=True, jupyter=jupyter, options=options)); 
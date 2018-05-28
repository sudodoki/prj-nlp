from collections import OrderedDict
import logging

def uas(trees, golden_trees, log = False):
    total, tp = 0, 0
    for tree, golden in zip(trees, golden_trees):
        t_nodes = [(node["id"], node["head"]) for node in tree]
        g_nodes = [(node["id"], node["head"]) for node in golden]
        total += len(tree)
        tp += len(set(g_nodes).intersection(set(t_nodes)))
    uas = tp/total
    result = {"Total:": total, "Correctly defined:": tp, "UAS:": round(uas, 2) }
    if log:
        for label, x in result.items():
            print(label, x)
    return uas

def uas_parse(golden_trees, dep_parse, log = False, trees = None):
    total, tp = 0, 0
    if trees == None:
        trees = golden_trees
    for tree, g_tree in zip(trees, golden_trees):
        t_nodes = dep_parse(tree)
        g_nodes = [(node["id"], node["head"]) for node in g_tree]
        total += len(tree)
        tp += len(set(g_nodes).intersection(set(t_nodes)))
    uas = tp/total
    result = {"Total:": total, "Correctly defined:": tp, "UAS:": round(uas, 2) }
    if log:
        for label, x in result.items():
            print(label, x)
    return uas    

def las_w(trees, golden_trees, log = False):
    total, tp = 0, 0
    for tree, golden in zip(trees, golden_trees):
        t_nodes = [(node["id"], node["head"], node["deprel"]) for node in tree]
        g_nodes = [(node["id"], node["head"], node["deprel"]) for node in golden]
        total += len(tree)
        tp += len(set(g_nodes).intersection(set(t_nodes)))
    las_w = tp/total
    result = {"Total:": total, "Correctly defined:": tp, "LAS(word):": round(las_w, 2) }
    if log:
        for label, x in result.items():
            print(label, x)
    return las_w    

def las_s(trees, golden_trees, log = False):
    tp = 0
    for tree, golden in zip(trees, golden_trees):
        length = len(tree)
        if length == 0:
            continue
        t_nodes = [(node["id"], node["head"], node["deprel"]) for node in tree]
        g_nodes = [(node["id"], node["head"], node["deprel"]) for node in golden]
        correct = len(set(g_nodes).intersection(set(t_nodes)))
        tp += correct/length
    las_s = tp/max(1, len(trees))
    result = {"Total sentences:": len(trees), "LAS(sentence):": round(las_s, 2) }
    if log:
        for label, x in result.items():
            print(label, x)
    return las_s       

def print_tree(tree, node_str = None):
    for node in tree:
        form = '<None>'
        head_form = '<None>'
        other = ''
        try:
            head = node["head"]
            form = node["form"]
            if head != None:
                head_form = tree[head - 1]["form"] if head > 0 else "root"
            if node_str:
                other = " ({})".format(node_str(node))
        except:
            logging.exception('Error in a tree.')
        print("{} <-- {}{}".format(form, head_form, other))
            
def print_tree_deprel(tree):
    print_tree(tree, lambda x: x["deprel"] if "deprel" in x else "---")
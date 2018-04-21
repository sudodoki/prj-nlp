from collections import OrderedDict
import conllu
from enum import Enum
from students.oleg_m.utils import stack

train_path = '/Users/admin/edu/NLP/uk_iu-ud-train.conllu'

with open(train_path, 'r') as dep_file:
    data_f = dep_file.read()

data = """
# doc_title = «Я обізвуся до них…»
# newdoc id = 0000
# newpar id = 0001
# sent_id = 0002
# text = У домі римського патриція Руфіна була прегарна фреска, зображення Венери та Адоніса.
1	У	у	ADP	Spsl	Case=Loc	2	case	_	Id=0003
2	домі	дім	NOUN	Ncmsln	Animacy=Inan|Case=Loc|Gender=Masc|Number=Sing	6	obl	_	Id=0004
3	римського	римський	ADJ	Ao-msgf	Case=Gen|Gender=Masc|Number=Sing	4	amod	_	Id=0005
4	патриція	патрицій	NOUN	Ncmsgy	Animacy=Anim|Case=Gen|Gender=Masc|Number=Sing	2	nmod	_	Id=0006
5	Руфіна	Руфін	PROPN	Npmsgy	Animacy=Anim|Case=Gen|Gender=Masc|NameType=Giv|Number=Sing	4	flat:title	_	Id=0007
6	була	бути	VERB	Vapis-sf	Aspect=Imp|Gender=Fem|Mood=Ind|Number=Sing|Tense=Past|VerbForm=Fin	0	root	_	Id=0008
7	прегарна	прегарний	ADJ	Ao-fsns	Case=Nom|Gender=Fem|Number=Sing	8	amod	_	Id=0009
8	фреска	фреска	NOUN	Ncfsnn	Animacy=Inan|Case=Nom|Gender=Fem|Number=Sing	6	nsubj	_	Id=000a|SpaceAfter=No
9	,	,	PUNCT	U	_	10	punct	_	Id=000b
10	зображення	зображення	NOUN	Ncnsnn	Animacy=Inan|Case=Nom|Gender=Neut|Number=Sing	8	appos	_	Id=000c
11	Венери	Венера	PROPN	Npfsgy	Animacy=Anim|Case=Gen|Gender=Fem|NameType=Giv|Number=Sing	10	nmod	_	Id=000d
12	та	та	CCONJ	Ccs	_	13	cc	_	Id=000e
13	Адоніса	Адоніс	PROPN	Npmsgy	Animacy=Anim|Case=Gen|Gender=Masc|NameType=Giv|Number=Sing	11	conj	_	Id=000f|SpaceAfter=No
14	.	.	PUNCT	U	_	6	punct	_	Id=000g
"""

tree = conllu.parse(data)
print(tree)


class Actions(str, Enum):
    SHIFT = "shift"
    REDUCE = "reduce"
    RIGHT = "right"
    LEFT = "left"


def get_action(top_stack, top_queue, relations):
    # check if both stack and queue are non-empty
    if top_stack and not top_queue:
        return Actions.REDUCE
    # check if there are any clear dependencies
    elif top_queue["head"] == top_stack["id"]:
        return Actions.RIGHT
    elif top_stack["head"] == top_queue["id"]:
        return Actions.LEFT
    # check if we can reduce the top of the stack
    elif top_stack["id"] in [i[0] for i in relations] and top_queue["head"] < top_stack["id"]:
        return Actions.REDUCE
    # default option
    else:
        return Actions.SHIFT


ROOT = OrderedDict([('id', 0), ('form', 'ROOT'), ('lemma', 'ROOT'), ('upostag', None),
                    ('xpostag', None), ('feats', None), ('head', None), ('deprel', None),
                    ('deps', None), ('misc', None)])

def prepare_features(stack, queue):
    pass


def trace_actions(tree, log=True):
    stack, queue, relations = [ROOT], tree[:], []

    while queue or stack:
        action = get_action(stack[-1] if len(stack) > 0 else None,
                            queue[0] if len(queue) > 0 else None,
                            relations)
        if log:
            print("Stack:", [i["form"] for i in stack])
            print("Queue:", [i["form"] for i in queue])
            print("Relations:", relations)
            print(action)
            print("========================")
        if action == Actions.SHIFT:
            stack.append(queue.pop(0))
        elif action == Actions.REDUCE:
            stack.pop()
        elif action == Actions.LEFT:
            relations.append((stack[-1]["id"], queue[0]["id"]))
            stack.pop()
        elif action == Actions.RIGHT:
            relations.append((queue[0]["id"], stack[-1]["id"]))
            stack.append(queue.pop(0))
        else:
            print("Unknown action.")
    if log:
        print("Gold relations:")
        print([(node["id"], node["head"]) for node in tree])
        print("Retrieved relations:")
        print(sorted(relations))


trace_actions(tree[0])
print(tree)


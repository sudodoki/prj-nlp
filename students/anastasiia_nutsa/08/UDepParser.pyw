from conllu import parse
from collections import OrderedDict
from enum import Enum
from sklearn.feature_extraction import DictVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn import svm
from sklearn.metrics import classification_report
from sklearn.preprocessing import label_binarize
from sklearn.multiclass import OneVsRestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.naive_bayes import GaussianNB
from sklearn.preprocessing import Imputer
import pymorphy2
import tokenize_uk

ROOT = OrderedDict([('id', 0), ('form', 'ROOT'), ('lemma', 'ROOT'), ('upostag', None),
                    ('xpostag', None), ('feats', None), ('head', None), ('deprel', None),
                    ('deps', None), ('misc', None)])

mapping = {"ADJF": "ADJ", "ADJS": "ADJ", "COMP": "ADJ", "PRTF": "ADJ",
           "PRTS": "ADJ", "GRND": "VERB", "NUMR": "NUM", "ADVB": "ADV",
           "NPRO": "PRON", "PRED": "ADV", "PREP": "ADP", "PRCL": "PART"}

CONJ_COORD = ["а", "або", "але", "ані", "все", "все-таки", "втім", "ж", "же",
              "зате", "і", "й", "ніже", "однак", "одначе", "прецінь", "проте",
              "та", "так", "також", "усе", "усе-таки", "утім", "чи"]

def normalize_pos(word):
    if word.tag.POS == "CONJ":
        if word.word in CONJ_COORD:
            return "CCONJ"
        else:
            return "SCONJ"
    else:
        return mapping.get(word.tag.POS, word.tag.POS)

class Actions(str, Enum):
    SHIFT = "shift"
    REDUCE = "reduce"
    RIGHT = "right"
    LEFT = "left"

def choose_action(s, q, r):
    lst = [x[0] for x in r]
    if s and not q:
        return Actions.REDUCE
    if s['head'] == q['id']:
        return Actions.LEFT
    elif s['id'] == q['head']:
        return Actions.RIGHT
    elif s['id'] in lst and s['id'] > q['head']:
        return Actions.REDUCE
    else:
        return Actions.SHIFT

def execute_action(stack, queue, relations, action, log = False):
    if action == Actions.LEFT:
        if log:
            relations.append((stack[-1]['form'], "<---", queue[0]['form']))
        else:
            relations.append((stack[-1]["id"], queue[0]["id"]))
        stack.pop()
    elif action == Actions.RIGHT:
        if log:
            relations.append((queue[0]['form'], "<---", stack[-1]['form']))
        else:    
            relations.append((queue[0]["id"], stack[-1]["id"]))
        stack.append(queue.pop(0))
    elif action == Actions.REDUCE:
        if len(stack) == 1 and queue:
            stack.append(queue.pop(0))
        else:    
            stack.pop()
    elif action == Actions.SHIFT:
        stack.append(queue.pop(0))
    else:
        print('Undefined behaviour')
##    if log:    
##        print(action, relations)    

def extract_features(stack, queue):
    features = dict()
    if len(stack) > 0:
        stack_top = stack[-1]
        features["s0-word"] = stack_top["form"]
        features["s0-lemma"] = stack_top["lemma"]
        features["s0-tag"] = stack_top["upostag"]
        if stack_top["feats"]:
            for k, v in stack_top["feats"].items():
                features["s0-" + k] = v
    if len(stack) > 1:
        features["s1-tag"] = stack_top["upostag"]
    if queue:
        queue_top = queue[0]
        features["q0-word"] = queue_top["form"]
        features["q0-lemma"] = queue_top["lemma"]
        features["q0-tag"] = queue_top["upostag"]
        if queue_top["feats"]:
            for k, v in queue_top["feats"].items():
                features["q0-" + k] = v
    if len(queue) > 1:
        queue_next = queue[1]
        features["q1-word"] = queue_next["form"]
        features["q1-tag"] = queue_next["upostag"]
    if len(queue) > 2:
        features["q2-tag"] = queue[2]["upostag"]
    if len(queue) > 3:
        features["q3-tag"] = queue[3]["upostag"]
    if stack and queue:
        features["distance"] = queue[0]["id"] - stack[-1]["id"]
    return features

def prepare_data(filename):
    with open(filename, 'r', encoding='utf-8') as f:
        data = f.read()
    trees = parse(data)
    return get_data(trees)

def get_data(trees):
    features, labels = [], []
    for tree in trees:
        stack, queue, relations = [ROOT], tree[:], []
        while stack or queue:
            action = choose_action(stack[-1] if stack else None, queue[0] if queue else None, relations)
            features.append(extract_features(stack, queue))
            labels.append(action.value)
            execute_action(stack, queue, relations, action)
    return features, labels, trees

def dep_parse(sentence, oracle, vectorizer, imputer, log = False):
    stack, queue, relations = [ROOT], sentence[:], []
    while stack or queue:
        if stack and not queue:
            stack.pop()
        else:
            features = extract_features(stack, queue)
            vectorized_features = vectorize_data([features], vectorizer, imputer)
            action = Actions(oracle.predict(vectorized_features))
            execute_action(stack, queue, relations, action, log)
    if log:
        return relations
    else:
        return sorted(relations)

def vectorize_data(data, vectorizer, imputer):
    return imputer.transform(vectorizer.transform(data))

train_features, train_labels, train_trees = prepare_data('uk_iu-ud-train.conllu')
test_features, test_labels, test_trees = prepare_data('uk_iu-ud-test.conllu')

vectorizer = DictVectorizer()
imputer = Imputer()
vec = vectorizer.fit(train_features + test_features)
imp = imputer.fit(vectorizer.transform(train_features + test_features))

train_features_vectorized = vectorize_data(train_features, vec, imp)
test_features_vectorized = vectorize_data(test_features, vec, imp)

##print("Naive Bayes Classifier")
##gnb = GaussianNB().fit(train_features_vectorized.toarray(), train_labels)
##predicted = gnb.predict(test_features_vectorized.toarray())
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

##print("K-Nearest Neighbours Classifier")
##knn = KNeighborsClassifier(n_neighbors = 7).fit(train_features_vectorized, train_labels)
##predicted = knn.predict(test_features_vectorized)
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

##print("Descision Tree Classifier")
##dtc = DecisionTreeClassifier(max_depth = 2).fit(train_features_vectorized, train_labels)
##predicted = dtc.predict(test_features_vectorized)
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

##print("SVM Classifier")
##svc = OneVsRestClassifier(svm.LinearSVC(random_state=7, C=0.1, dual=False))
##svc.fit(train_features_vectorized, train_labels)
##predicted = svc.predict(test_features_vectorized)
##print(classification_report(test_labels, predicted))
##print('\n' + 50*'#' + '\n')

print("Logistic Regression Classifier")
lrc = LogisticRegression(random_state=0, multi_class='multinomial', solver='saga', C=0.05, n_jobs=-1, max_iter=1500)
lrc.fit(train_features_vectorized, train_labels)
predicted = lrc.predict(test_features_vectorized)
print(classification_report(test_labels, predicted))

def get_tree(sent):
    result = []
    words = tokenize_uk.tokenize_words(sent)
    for i in range(0, len(words)):
        p = morph.parse(words[i])[0]
        feats = OrderedDict()
        if p.tag.animacy:
            feats["Animacy"] = p.tag.animacy.title()
        if p.tag.case:
            feats["Case"] = p.tag.case[0:3].title()
        if p.tag.gender:
            feats["Gender"] = p.tag.gender.title()
        if p.tag.number:
            feats["Number"] = p.tag.number.title()                        
        result.append(OrderedDict([('id', i+1), ('form', words[i]), ('lemma', p.normal_form), ('upostag', normalize_pos(p)),
                                   ('xpostag', None), ('feats', feats), ('head', None), ('deprel', None),
                                   ('deps', None), ('misc', OrderedDict())]))
    return result

morph = pymorphy2.MorphAnalyzer(lang='uk')

tmp_trees = ['Походження міста Львів оповито пеленою великих таємниць.',
             'Як видно з обговорення, львів\'яни не цікавляться поглибленням історії свого міста на тисячу років!',
             'За два тижні користування можу впевнено сказати, що телефоном задоволена.',
             'Сподіваємось, ви знайдете ті подіїї, які зацікавлять саме вас.',
             'Все, що література повинна виховувати у дітей – це естетичний смак, і більше нічого.']

for item in tmp_trees:
    tree = get_tree(item)
    print([node["form"] for node in tree])
    print(dep_parse(tree, lrc, vec, imp, True))
    print('\n')

total, tp = 0, 0
for tree in test_trees:
    golden = [(node["id"], node["head"]) for node in tree]
    predicted = dep_parse(tree, lrc, vec, imp)
    total += len(tree)
    tp += len(set(golden).intersection(set(predicted)))

print("Total:", total)
print("Correctly defined:", tp)
print("UAS:", round(tp/total, 2))
    


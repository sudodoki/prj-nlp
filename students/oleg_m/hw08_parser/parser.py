from collections import OrderedDict
from conllu import parse
from enum import Enum
import pandas as pd
from sklearn.feature_extraction import DictVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import classification_report, confusion_matrix
# from sklearn.metrics import accuracy_score, precision_score, recall_score, confusion_matrix, precision_recall_fscore_support, f1_score, roc_auc_score, roc_curve, auc


PATH = "/Users/admin/edu/NLP/"

ROOT = OrderedDict([('id', 0), ('form', 'ROOT'), ('lemma', 'ROOT'), ('upostag', 'ROOT'),
                    ('xpostag', None), ('feats', None), ('head', None), ('deprel', None),
                    ('deps', None), ('misc', None)])


class Actions(str, Enum):
    SHIFT = "shift"
    REDUCE = "reduce"
    RIGHT = "right"
    LEFT = "left"


def oracle(top_stack, top_queue, relations):
    """
    Make a decision on the right action to do.
    """
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


def extract_features(stack, queue, sent_len):
    features = dict()
    if len(stack) > 0:
        stack_top = stack[-1]
        features["s0-word"] = stack_top["form"]
        features["s0-lemma"] = stack_top["lemma"]
        features["s0-tag"] = stack_top["upostag"]
        features["s0-len"] = len(stack_top["form"])
        features["s0-start"] = stack_top["id"] == 0
        features["s0-eos"] = sent_len - stack_top["id"] == 2
        features["s0-posit"] = stack_top["id"] + 1
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
        features["q0-len"] = len(queue_top["form"])
        features["q0-start"] = queue_top["id"] == 0
        features["q0-eos"] = sent_len - queue_top["id"] == 2
        features["q0-posit"] = queue_top["id"] + 1
        if queue_top["feats"]:
            for k, v in queue_top["feats"].items():
                features["q0-" + k] = v
    if len(queue) > 1:
        queue_next = queue[1]
        features["q1-word"] = queue_next["form"]
        features["q1-tag"] = queue_next["upostag"]
        features["q0-bef_punct"] = queue_next["upostag"] == "PUNCT" and queue_next["form"] != "."
    if len(queue) > 2:
        features["q2-tag"] = queue[2]["upostag"]
    if len(queue) > 3:
        features["q3-tag"] = queue[3]["upostag"]
    if stack and queue:
        features["distance"] = queue[0]["id"] - stack[-1]["id"]
        features["is-neib"] = abs(queue[0]["id"] - stack[-1]["id"]) == 1
    return features


def trace_actions(tree, log=True):
    """
    Try out the oracle to verify it's returning the right actions.
    """
    stack, queue, relations = [ROOT], tree[:], []
    while queue or stack:
        action = oracle(stack[-1] if len(stack) > 0 else None,
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


def get_data(trees):
    features, labels = [], []
    for tree in trees:
        stack, queue, relations = [ROOT], tree[:], []

        while queue or stack:
            action = oracle(stack[-1] if len(stack) > 0 else None,
                            queue[0] if len(queue) > 0 else None,
                            relations)
            features.append(extract_features(stack, queue, len(tree)))
            labels.append(action.value)
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
    return features, labels


with open(PATH + "/uk_iu-ud-train.conllu", "r") as f:
    data = f.read()

trees = parse(data)
tree = trees[0]
# trace_actions(tree)
# features, labels = get_data([tree])
train_features, train_labels = get_data(trees)

with open(PATH + "/uk_iu-ud-test.conllu", "r") as f:
    data_test = f.read()

test_trees = parse(data_test)
test_features, test_labels = get_data(test_trees)
print(len(test_features), len(test_labels))

vectorizer = DictVectorizer()
vec = vectorizer.fit(train_features + test_features)

print("\nTotal number of features: ", len(vec.get_feature_names()))

train_features_vectorized = vec.transform(train_features)
test_features_vectorized = vec.transform(test_features)

print(len(train_features_vectorized.toarray()), len(test_features_vectorized.toarray()))

lrc = LogisticRegression(random_state=42)
lrc.fit(train_features_vectorized, train_labels)
predicted = lrc.predict(test_features_vectorized)

print(classification_report(test_labels, predicted))

classes = ['left', 'reduce', 'right', 'shift']
conf_matrix = confusion_matrix(test_labels, predicted, labels=classes)
pd.DataFrame(conf_matrix, columns=classes, index=classes)
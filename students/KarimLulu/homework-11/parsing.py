from collections import OrderedDict
import numpy as np

from helpers import clean_deprel

ROOT = OrderedDict([('id', 0), ('form', 'ROOT'), ('lemma', 'ROOT'), ('upostag', "ROOT"),
                    ('xpostag', None), ('feats', None), ('head', None), ('deprel', None),
                    ('deps', None), ('misc', None)])

def get_parse_context(word, deps, data):
    if not word or word == -1:
        return 0, "", ""
    deps = deps[word["id"]]
    num = len(deps)
    if not num:
        return num, "", ""
    elif num==1:
        return num, data[deps[-1]-1], ""
    else:
        return num, data[deps[-1]-1], data[deps[-1]-1]

def extract_features(stack, queue, tree, parse):
    features = {}
    stack_depth = len(stack)
    s0 = stack[-1] if stack_depth else ""
    q0 = queue[0] if queue else ""

    # Features for stack
    if stack:
        features["s0-form"] = s0["form"]
        features["s0-tag"] = s0["upostag"]
        features["s0-lemma"] = s0["lemma"]
        features["s0-word-tag"] = s0["form"] + s0["upostag"]
        if s0.get("feats"):
            for k, v in s0["feats"].items():
                features[f"s0-{k}"] = v
    if stack_depth > 1:
        features["s1-tag"] = stack[-2]["upostag"]
        features["s1-word-tag"] = stack[-2]["form"] + stack[-2]["upostag"]

    # Features for queue
    if queue:
        features["q0-form"] = q0["form"]
        features["q0-tag"] = q0["upostag"]
        features["q0-lemma"] = q0["lemma"]
        features["q0-word-tag"] = q0["form"] + q0["upostag"]
        if q0.get("feats"):
            for k, v in q0["feats"].items():
                features[f"q0-{k}"] = v
    if len(queue) > 1:
        features["q1-form"] = queue[1]["form"]
        features["q1-tag"] = queue[1]["upostag"]
        features["q1-word-tag"] = queue[1]["form"] + queue[1]["upostag"]
        features["q0q1"] = q0["form"] + queue[1]["form"]
    if len(queue) > 2:
        features["q2-tag"] = queue[2]["upostag"]
        #features["q2-word-tag"] = queue[2]["form"] + queue[2]["upostag"]
    if len(queue) > 3:
        features["q3-tag"] = queue[3]["upostag"]

    if queue and stack:
        Ds0q0 = q0["id"] - s0["id"]
        features["distance"] = Ds0q0
        features["q0-dist"] = q0["form"] + "-{}".format(Ds0q0)
        features["s0-dist"] = s0["form"] + "-{}".format(Ds0q0)
        features["s0q0-dist"] = s0["lemma"] + q0["lemma"] + "-{}".format(Ds0q0)
        features["s0-tag-dist"] = s0["upostag"] + "-{}".format(Ds0q0)
        features["q0-tag-dist"] = q0["upostag"] + "-{}".format(Ds0q0)
        features["s0q0-tag-dist"] = s0["upostag"] + q0["upostag"] + "-{}".format(Ds0q0)
        # Add bigrams
        features["s0q0"] = s0["form"] + q0["form"]
        features["s0q0-tag"] = s0["upostag"] + q0["upostag"]
        features["q0_q0-tag_s0"] = q0["form"] + q0["upostag"] + s0["form"]
        features["q0_q0-tag_s0-tag"] = q0["form"] + q0["upostag"] + s0["upostag"]
        features["s0_s0-tag_q0"] = s0["form"] + s0["upostag"] + q0["form"]
        features["s0_s0-tag_q0-tag"] = s0["form"] + s0["upostag"] + q0["upostag"]
        features["s0_s0-tag_q0_q0-tag"] = s0["form"] + s0["upostag"] + q0["form"] + q0["upostag"]



    # Left two child for top stack
    Ns0l, s0l1, s0l2 = get_parse_context(s0, parse.lefts, tree)
    if s0l1:
        features["s0l1"] = s0l1["form"]
        features["s0l1-tag"] = s0l1["upostag"]
    if s0l2:
        features["s0l2"] = s0l2["form"]
        features["s0l2-tag"] = s0l2["upostag"]

    # Right two child for top stack
    Ns0r, s0r1, s0r2 = get_parse_context(s0, parse.rights, tree)
    if s0r1:
        features["s0r1"] = s0r1["form"]
        features["s0r1-tag"] = s0r1["upostag"]
    if s0r2:
        features["s0r2"] = s0r2["form"]
        features["s0r2-tag"] = s0r2["upostag"]

    # Left two child for top queue
    Nq0l, q0l1, q0l2 = get_parse_context(q0, parse.lefts, tree)
    if q0l1:
        features["q0l1"] = q0l1["form"]
        features["q0l1-tag"] = q0l1["upostag"]
    if q0l2:
        features["q0l2"] = q0l2["form"]
        features["q0l2-tag"] = q0l2["upostag"]

    if stack:
        features["s0l-N"] = s0["form"] + f"-{Ns0l}"
        features["s0r-N"] = s0["form"] + f"-{Ns0r}"
        features["s0l-tag-N"] = s0["upostag"] + f"-{Ns0l}"
        features["s0r-tag-N"] = s0["upostag"] + f"-{Ns0r}"
    if queue:
        features["q0l-N"] = q0["form"] + f"-{Nq0l}"
        features["q0l-tag-N"] = q0["upostag"] + f"-{Nq0l}"
    return features


class Parse(object):

    def __init__(self, n):
        self.n = n
        self.relations = []
        self.lefts = []
        self.rights = []
        # we need n+1 coz examples in the training data are indexed from 1
        for k in range(n+1):
            self.lefts.append([])
            self.rights.append([])

    def add_relation(self, child, head, deprel=None):
        self.relations.append((child, head, deprel))
        if child < head:
            self.lefts[head].append((child, deprel))
        else:
            self.rights[head].append((child, deprel))

class Parser(object):

    def __init__(self, label_index={}):
        self.label_index = label_index

    @property
    def idx_2_label(self):
        return {v:k for k,v in self.label_index.items()}

    def get_action(self, stack, q, parse):
        if stack and not q:
            return "reduce"
        if stack[-1]["head"] == q[0]["id"] and (stack[-1]["id"] not in
            [child for child, _, _ in parse.relations]):
            return "left"
        elif q[0]["head"] == stack[-1]["id"]:
            return "right"
        elif (stack[-1]["head"] in [parent for _, parent, _ in parse.relations]
              and q[0]["head"] < stack[-1]["id"]
             ):
            return "reduce"
        else:
            return "shift"

    def parse(self, tree, oracle=None, log=False, feature_extractor=extract_features,
              labeled=True, update_label_index=True):
        q = tree.copy()
        parse = Parse(len(q))
        stack = [ROOT]
        labels = []
        features = []
        while q or stack:
            if log:
                print("Stack:", [el["form"] for el in stack])
                print("Q:", [el["form"] for el in q])
            feature_set, n_w, n_t, n_d, n_num = feature_extractor(stack, q, tree, parse)

            if X is None:
                X = [[0]*len(feature_set) for _ in range(5)]

            deprel = ""
            if oracle is not None:
                X.append(feature_set)
                X1 = np.array([X])[:, -5:, :]
                probas = oracle.predict([X1[:, :, :n_w], X1[:, :, n_w:n_w+n_t], X1[:, :, n_w+n_t:n_w+n_t+n_d], X1[:, :, n_w+n_t+n_d:]])[0][-1]
                pred_action_id = np.argmax(probas[:-1])
                pred_action_id = np.argmax(probas)
                action = self.idx_2_label[pred_action_id]
                if "left" in action or "right" in action:
                    action, deprel = action.split("_")
            else:
                action = self.get_action(stack or None, q or None, parse)

            if action == "left":
                if not deprel:
                    deprel = stack[-1]["deprel"]
                parse.add_relation(stack[-1]["id"], q[0]["id"], deprel)
                stack.pop()
            elif action == "right":
                if not deprel:
                    deprel = q[0]["deprel"]
                parse.add_relation(q[0]["id"], stack[-1]["id"], deprel)
                stack.append(q.pop(0))
            elif action == "reduce":
                stack.pop()
            elif action == "shift":
                stack.append(q.pop(0))

            if deprel and labeled:
                action = f"{action}_{deprel}"

            if update_label_index:
                action_id = len(self.label_index)
                self.label_index[action] = self.label_index.get(action, action_id)

            labels.append(self.label_index[action])
            features.append(feature_set)
        return labels, features, parse.relations, n_w, n_t, n_d, n_num

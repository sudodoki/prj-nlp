from enum import Enum
from sklearn.feature_extraction import DictVectorizer
from sklearn.linear_model import LogisticRegression
from collections import OrderedDict
from sklearn.externals import joblib
import logging
from collections import deque


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
    elif top_stack["id"] in [i[0] for i in relations] and \
         top_queue["head"] < top_stack["id"]:
        return Actions.REDUCE
    # default option
    else:
        return Actions.SHIFT

def get_root():
    return OrderedDict([('id', 0), ('form', 'ROOT'), ('lemma', 'ROOT'), ('upostag', 'ROOT'),
                ('xpostag', None), ('feats', None), ('head', None), ('deprel', None),
                ('deps', None), ('misc', None)])


def trace_actions(tree, ROOT, oracle, log=True):
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
            logging.warning("Unknown action.")
    if log:
        print("Gold relations:")
        print([(node["id"], node["head"]) for node in tree])
        print("Retrieved relations:")
        print(sorted(relations))


def get_data(trees, oracle, feature_extractor, get_root):
    features, labels = [], []
    for tree in trees:
        feature_extractor.start_sentence(tree)
        stack, queue, relations = [get_root()], tree[:], []

        while queue or stack:
            action = oracle(stack[-1] if len(stack) > 0 else None,
                            queue[0] if len(queue) > 0 else None,
                            relations)
            features.append(feature_extractor.extract_features(stack, queue))
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
                logging.warning("Unknown action.")
    return features, labels  

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


class FeatureExtractor():
    def start_sentence(self, sentence):
        self.sentence = sentence

    def extract_features(self, stack, queue):
        return extract_features(stack, queue)  


class MemoryFeatureExtractor():
    def start_sentence(self, sentence):
        self.sentence = sentence
        self.history = deque()
    
    def extract_features(self, stack, queue):
        features = extract_features(stack, queue)
        stack_0 = stack[-1] if stack and len(stack) > 0 else None
        stack_1 = stack[-2] if stack and len(stack) > 1 else None
        queue_0 = queue[0] if queue and len(queue) > 0 else None
        queue_1 = queue[1] if queue and len(queue) > 1 else None
        queue_2 = queue[2] if queue and len(queue) > 2 else None
        #ngrams
        features['q-0-1'] = "{}_{}".format(queue_0['upostag'] if queue_0 else '', queue_1['upostag'] if queue_1 else '')
        features['s-0-1'] = "{}_{}".format(stack_0['upostag'] if stack_0 else '', stack_1['upostag'] if stack_1 else '')
        if stack_0:
            idx = stack_0['id']-1
            s_left = self.sentence[idx - 1] if idx > 0 else None
            s_right = self.sentence[idx + 1] if idx + 1 < len(self.sentence) else None
            features['s-3gram'] = "{}_{}_{}".format(s_left['upostag'] if s_left else '', stack_0['upostag'],
                                                   s_right['upostag'] if s_right else '')
            features['s-2gram-left'] = "{}_{}".format(s_left['upostag'] if s_left else '', stack_0['upostag'])
            features['s-2gram-right'] = "{}_{}".format(stack_0['upostag'], s_right['upostag'] if s_right else '')   
        if queue_0:
            idx = queue_0['id']-1
            q_left = self.sentence[idx - 1] if idx > 0 else None
            q_right = self.sentence[idx + 1] if idx + 1 < len(self.sentence) else None
            features['q-3gram'] = "{}_{}_{}".format(q_left['upostag'] if q_left else '', stack_0['upostag'],
                                                   q_right['upostag'] if q_right else '')
            features['q-2gram-left'] = "{}_{}".format(q_left['upostag'] if q_left else '', stack_0['upostag'])
            features['q-2gram-right'] = "{}_{}".format(stack_0['upostag'], q_right['upostag'] if q_right else '')                                                          
        #history
        for i, x in enumerate(self.history):
            features['history-'+str(i)] = x
        if len(self.history) > 1:
            self.history.popleft()
        self.history.append("{}_{}".format(stack_0['upostag'] if stack_0 else '', queue_0['upostag'] if queue_0 else ''))
        return features         


def update_relations(tree, relations, normalize = True):
    for i, head in relations:
        tree[i-1]['head'] = head
    if normalize:
        for node in tree:
            if not node['head']:
                node['head'] = 0
    return tree

 
class ArcEagerDependencyParser():
    def __init__(self, oracle=oracle, get_root=get_root, get_data=get_data, feature_extractor = None,
        classifier=None, vectorizer=None):
        self.oracle = oracle
        self.get_root = get_root
        self.feature_extractor = feature_extractor if feature_extractor else FeatureExtractor()
        self.get_data = get_data
        self.classifier = classifier
        self.vectorizer = vectorizer

    def train(self, train_trees, classifier = None, vectorizer = None):
        self.classifier = classifier if classifier != None else LogisticRegression(random_state=42)
        self.vectorizer = vectorizer if vectorizer != None else DictVectorizer()

        train_features, train_labels = self.get_data(train_trees, self.oracle, self.feature_extractor, self.get_root)
        if vectorizer == None:
            self.vectorizer.fit(train_features)
        train_features_vectorized = self.vectorizer.transform(train_features)
        self.classifier.fit(train_features_vectorized, train_labels)

    def test(self, test_trees):
        test_features, test_labels = self.get_data(test_trees, self.oracle, self.feature_extractor, self.get_root)
        test_features_vectorized = self.vectorizer.transform(test_features)
        predicted = self.classifier.predict(test_features_vectorized)
        return predicted, test_labels

    def dep_parse(self, sentence):
        stack, queue, relations = [self.get_root()], sentence[:], []
        self.feature_extractor.start_sentence(sentence)
        while queue or stack:
            if stack and not queue:
                stack.pop()
            else:
                features = self.feature_extractor.extract_features(stack, queue)
                action = self.classifier.predict(self.vectorizer.transform([features]))[0]
                # actual parsing
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
                    logging.warning("Unknown action.")
        return sorted(relations)

    def save(self, vectorizer_fname, classifier_fname):
        joblib.dump(self.vectorizer, vectorizer_fname)
        joblib.dump(self.classifier, classifier_fname)         

    def load(self, vectorizer_fname, classifier_fname):
        self.vectorizer = joblib.load(vectorizer_fname)
        self.classifier = joblib.load(classifier_fname)           







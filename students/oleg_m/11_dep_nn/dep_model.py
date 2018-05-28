import numpy as np
from conllu import parse


DATA_DIR = '/Users/admin/edu/NLP/practical_NLP_course/'

word_dict = {}

with open(DATA_DIR+'fiction.lowercased.tokenized.300d') as emb_file:
    lines = emb_file.readlines()

for line in lines[1:]:
    word, arr = line.split(maxsplit=1)
    word_dict[word] = [float(x) for x in arr.split()]
    # word_dict[word] = np.array([float(x) for x in arr.split()])


with open(DATA_DIR + "uk_iu-ud-train.conllu", "r") as f:
    data = f.read()

trees = parse(data)

unknown_word = [0.0]*300
x_train = np.ndarray(shape=(1, 300), dtype=float)
y_train = []


with open(DATA_DIR + "uk_iu-ud-test.conllu", "r") as f:
    data_test = f.read()

trees_test = parse(data_test)

with open(DATA_DIR + 'test_vectors.csv', 'a') as output:
    for tree in trees_test:
        for node in tree:
            head = node["head"]
            child_token = node["form"].lower()
            head_token = (tree[head - 1]["form"] if head > 0 else "root").lower()
            head_vector = word_dict[head_token] if head_token in word_dict else unknown_word
            child_vector = word_dict[child_token] if child_token in word_dict else unknown_word
            if head_vector == unknown_word and child_vector == unknown_word:
                continue
            write_str = ','.join(str(x) for x in head_vector+child_vector+[node["deprel"]])
            output.write(write_str + '\n')

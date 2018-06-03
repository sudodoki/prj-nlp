#trying pytorch
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import DataLoader, Dataset
from bz2 import BZ2File
from collections import OrderedDict
from conllu import parse
import pandas as pd
import numpy as np
from sklearn.preprocessing import LabelEncoder
from sklearn import metrics
from tqdm import tqdm

embeddings = "fiction.lowercased.tokenized.word2vec.300d.bz2"

ROOT = OrderedDict([('id', 0), ('form', 'ROOT'), ('lemma', 'ROOT'), ('upostag', 'ROOT'),
                    ('xpostag', None), ('feats', None), ('head', None), ('deprel', None),
                    ('deps', None), ('misc', None)])

def read_trees(filename):
    with open(filename) as input_file:
        text = input_file.read()
        result = parse(text)
    return result

def load_emb(path):
    voc = {}
    words = {}
    dim = None
    i = 0
    with BZ2File(path) as f:
        for line in f:
            if i == 0:
                sizes = [int(s.strip()) for s in line.split(' ')]
                dim = np.zeros(sizes)
            else:
                chunks = line.split(' ', maxsplit=1)
                word, num_string = chunks[0], chunks[1]
                nums = np.fromstring(num_string, sep=' ', dtype=np.float32)
                dim[i - 1, :] = nums
                voc[word] = i - 1
                words[i - 1] = word
            i += 1
    return dim, voc, words


def if_head(tok):
    return tok['head'] if 'head' in tok else 0


def vectorize(sentences, map2id, labeler):
    output = pd.DataFrame()
    for sentence in sentences:
        rels = [(tok['id'], if_head(tok)) for tok in sentence]
        toks = [ROOT] + sentence
        for (child, head) in rels:
            output = output.append({'head': map2id[toks[head]['lemma']] if toks[head]['lemma'] in map2id else 0,
                                    'child': map2id[toks[child]['lemma']] if toks[child]['lemma'] in map2id else 0,
                                    'deprel': toks[child]['deprel']}, ignore_index=True)
    return output[['child', 'head']].astype(int), labeler.transform(output['deprel'].astype(str))



def LAS(lab, pred):
    total = 0
    correct = 0
    for pair in zip(lab, pred):
        total += 1
        if pair[0] == pair[1]:
            correct += 1
    return correct / total

class RelationsData(Dataset):
    def __init__(self, Xs, ys):
        super().__init__()
        self.Xs = Xs
        self.ys = ys
        self.size = len(Xs)

    def __getitem__(self, index):
        x = self.Xs[index:index + 1].values[0]
        target = self.ys[index]
        return x, target

    def __len__(self):
        return self.size


class RelationModel(nn.Module):
    def __init__(self, embedding_matrix, output_size):
        super(RelationModel, self).__init__()

        vocab_size = embedding_matrix.shape[0]
        vector_size = embedding_matrix.shape[1]

        self.embeddings = nn.Embedding(vocab_size, vector_size)
        self.embeddings.weight.data.copy_(torch.from_numpy(embedding_matrix))
        self.dense1 = nn.Linear(vector_size * 2, 1000)
        self.activation1 = nn.ReLU()
        self.dense2 = nn.Linear(1000, output_size)
        self.activation2 = nn.Sigmoid()

    def forward(self, in1, in2):
        emb1 = self.embeddings(in1)
        emb2 = self.embeddings(in2)
        in_cat = torch.cat([emb1, emb2], 1)
        out = self.dense1(in_cat)
        out = self.activation1(out)
        out = self.dense2(out)
        y_pred = F.softmax(out, dim=1)
        return y_pred



if __name__ == "__main__":
    test = read_trees('uk_iu-ud-dev.conllu')
    val = read_trees('uk_iu-ud-test.conllu')
    train = read_trees('uk_iu-ud-train.conllu')
    dim, voc, words = load_emb(embeddings)

    deprels = set()
    for sent in train:
        for tok in sent:
            deprels.update([tok['deprel']])
    deprels = list(deprels)

    OUTPUT_SIZE = len(deprels)
    labeler = LabelEncoder()
    labeler.fit(deprels)

    batch_size = 512
    num_workers = 8
    lr = 0.000001
    num_epochs = 20

    X_train, y_train = vectorize(train, voc, labeler)
    X_test, y_test = vectorize(test, voc, labeler)

    ds_train = RelationsData(X_train, y_train)
    loader_train = DataLoader(ds_train, batch_size=batch_size, num_workers=num_workers, drop_last=True)

    ds_test = RelationsData(X_test, y_test)
    loader_test = DataLoader(ds_test, batch_size=batch_size, num_workers=num_workers)

    model = RelationModel(dim, output_size=OUTPUT_SIZE)
    criterion = nn.CrossEntropyLoss()

    optimizer = torch.optim.Adam(model.parameters(), lr=lr)

    for epoch in range(num_epochs):
        for step, (x, y) in enumerate(tqdm(loader_train)):
            x1 = x[:, 0].type(torch.LongTensor)
            x2 = x[:, 1].type(torch.LongTensor)

            y_pred = model(x1, x2)
            loss = criterion(y_pred, y)

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()
            if (step % 50) == 0 and step > 0:
                print(step, loss.item())
    print('last loss: ', loss.item())

    res = []
    model.eval()

    with torch.no_grad():
        for (x, target) in tqdm(loader_test):
            x1 = x[:, 0].type(torch.LongTensor)
            x2 = x[:, 1].type(torch.LongTensor)
            output = model(x1, x2).data.cpu().numpy()
            classes = np.argmax(output, axis=1)
            res = np.append(res, classes)
    result = res
    result = [int(x) for x in result]

    print(metrics.classification_report(y_test, result, target_names=labeler.classes_))
    print(LAS(y_test, result))


'''
input sizes:  [110363, 300]
read 110364 total lines

 35%|███▍      | 51/146 [00:12<00:22,  4.21it/s]50 3.8921515941619873
 69%|██████▉   | 101/146 [00:23<00:10,  4.26it/s]100 3.8918724060058594
100%|██████████| 146/146 [00:33<00:00,  4.30it/s]
 34%|███▍      | 50/146 [00:11<00:22,  4.19it/s]50 3.890737771987915
 68%|██████▊   | 100/146 [00:42<00:19,  2.34it/s]100 3.890511989593506
100%|██████████| 146/146 [00:58<00:00,  2.50it/s]
 34%|███▍      | 50/146 [00:14<00:27,  3.51it/s]50 3.889064073562622
 68%|██████▊   | 100/146 [00:26<00:12,  3.79it/s]100 3.8888864517211914
100%|██████████| 146/146 [00:37<00:00,  3.88it/s]
 35%|███▍      | 51/146 [00:12<00:22,  4.14it/s]50 3.8870372772216797
 68%|██████▊   | 100/146 [00:23<00:10,  4.22it/s]100 3.886889696121216
100%|██████████| 146/146 [00:34<00:00,  4.22it/s]
 34%|███▍      | 50/146 [00:12<00:23,  4.07it/s]50 3.884517192840576
 69%|██████▉   | 101/146 [00:24<00:10,  4.16it/s]100 3.8843486309051514
100%|██████████| 146/146 [00:34<00:00,  4.20it/s]
 34%|███▍      | 50/146 [00:12<00:23,  4.14it/s]50 3.881277561187744
 69%|██████▉   | 101/146 [00:22<00:10,  4.45it/s]100 3.8809759616851807
100%|██████████| 146/146 [00:32<00:00,  4.52it/s]
 34%|███▍      | 50/146 [00:10<00:20,  4.70it/s]50 3.876997947692871
 69%|██████▉   | 101/146 [00:21<00:09,  4.80it/s]100 3.8763983249664307
100%|██████████| 146/146 [00:30<00:00,  4.85it/s]
 35%|███▍      | 51/146 [00:10<00:19,  4.76it/s]50 3.8711633682250977
 69%|██████▉   | 101/146 [00:20<00:09,  4.85it/s]100 3.8699281215667725
100%|██████████| 146/146 [00:29<00:00,  4.89it/s]
 34%|███▍      | 50/146 [00:10<00:20,  4.71it/s]50 3.863041639328003
 69%|██████▉   | 101/146 [00:21<00:09,  4.77it/s]100 3.8607280254364014
100%|██████████| 146/146 [00:30<00:00,  4.84it/s]
 35%|███▍      | 51/146 [00:10<00:20,  4.74it/s]50 3.851905345916748
 68%|██████▊   | 100/146 [00:20<00:09,  4.84it/s]100 3.8480467796325684
100%|██████████| 146/146 [00:29<00:00,  4.89it/s]
 35%|███▍      | 51/146 [00:10<00:20,  4.73it/s]50 3.837400197982788
 68%|██████▊   | 100/146 [00:20<00:09,  4.84it/s]100 3.8321220874786377
100%|██████████| 146/146 [00:29<00:00,  4.88it/s]
 35%|███▍      | 51/146 [00:10<00:20,  4.73it/s]50 3.820115804672241
 68%|██████▊   | 100/146 [00:20<00:09,  4.81it/s]100 3.8143579959869385
100%|██████████| 146/146 [00:29<00:00,  4.88it/s]
 34%|███▍      | 50/146 [00:10<00:20,  4.74it/s]50 3.801337957382202
 68%|██████▊   | 100/146 [00:20<00:09,  4.81it/s]100 3.7964162826538086
100%|██████████| 146/146 [00:29<00:00,  4.87it/s]
 35%|███▍      | 51/146 [00:10<00:19,  4.75it/s]50 3.782217025756836
 68%|██████▊   | 100/146 [00:20<00:09,  4.82it/s]100 3.77921724319458
100%|██████████| 146/146 [00:29<00:00,  4.87it/s]
 35%|███▍      | 51/146 [00:10<00:20,  4.71it/s]50 3.763467788696289
 68%|██████▊   | 100/146 [00:20<00:09,  4.80it/s]100 3.763073444366455
100%|██████████| 146/146 [00:30<00:00,  4.84it/s]
 34%|███▍      | 50/146 [00:10<00:20,  4.61it/s]50 3.745523691177368
 69%|██████▉   | 101/146 [00:21<00:09,  4.74it/s]100 3.7480640411376953
100%|██████████| 146/146 [00:30<00:00,  4.82it/s]
 34%|███▍      | 50/146 [00:10<00:20,  4.70it/s]50 3.728537082672119
 68%|██████▊   | 100/146 [00:20<00:09,  4.80it/s]100 3.734123468399048
100%|██████████| 146/146 [00:30<00:00,  4.84it/s]
 34%|███▍      | 50/146 [00:10<00:20,  4.63it/s]50 3.712695360183716
 68%|██████▊   | 100/146 [00:21<00:09,  4.76it/s]100 3.721327304840088
100%|██████████| 146/146 [00:30<00:00,  4.81it/s]
 34%|███▍      | 50/146 [00:10<00:20,  4.73it/s]50 3.6980926990509033
 69%|██████▉   | 101/146 [00:21<00:09,  4.78it/s]100 3.709686756134033
100%|██████████| 146/146 [00:30<00:00,  4.83it/s]
 35%|███▍      | 51/146 [00:10<00:20,  4.67it/s]50 3.684684991836548
 68%|██████▊   | 100/146 [00:20<00:09,  4.77it/s]100 3.6991639137268066
100%|██████████| 146/146 [00:30<00:00,  4.84it/s]
last loss:  3.6293537616729736
100%|██████████| 21/21 [00:00<00:00, 44.23it/s]
/home/darth/PycharmProjects/prj-nlp/venv/lib/python3.6/site-packages/sklearn/metrics/classification.py:1428: UserWarning: labels size, 43, does not match size of target_names, 49
  .format(len(labels), len(target_names))
/home/darth/PycharmProjects/prj-nlp/venv/lib/python3.6/site-packages/sklearn/metrics/classification.py:1135: UndefinedMetricWarning: Precision and F-score are ill-defined and being set to 0.0 in labels with no predicted samples.
  'precision', 'predicted', average, warn_for)
                     precision    recall  f1-score   support

                acl       0.00      0.00      0.00       184
              advcl       0.00      0.00      0.00       109
           advcl:sp       0.00      0.00      0.00         4
          advcl:svc       0.00      0.00      0.00         6
             advmod       0.35      0.06      0.10       484
               amod       0.00      0.00      0.00       846
              appos       0.00      0.00      0.00        71
                aux       0.00      0.00      0.00        19
               case       0.33      0.99      0.49       945
                 cc       0.00      0.00      0.00       358
              ccomp       0.00      0.00      0.00        50
           compound       0.00      0.00      0.00        64
       compound:svc       0.00      0.00      0.00       475
               conj       0.00      0.00      0.00        57
           conj:svc       0.00      0.00      0.00        52
                cop       0.00      0.00      0.00       226
              csubj       0.00      0.00      0.00        12
                det       0.00      0.00      0.00         2
         det:numgov       0.00      0.00      0.00       139
         det:nummod       0.00      0.00      0.00         5
          discourse       0.00      0.00      0.00         7
         dislocated       0.00      0.00      0.00        26
               expl       0.00      0.00      0.00         3
              fixed       0.00      0.00      0.00        18
               flat       0.00      0.00      0.00        52
       flat:foreign       0.00      0.00      0.00         1
          flat:name       0.00      0.00      0.00       149
        flat:repeat       0.00      0.00      0.00        10
         flat:title       0.00      0.00      0.00       205
           goeswith       0.00      0.00      0.00      1044
               iobj       0.00      0.00      0.00       634
               list       0.00      0.00      0.00        24
               mark       0.00      0.00      0.00        43
               nmod       0.00      0.00      0.00        44
              nsubj       0.00      0.00      0.00       488
         nsubj:pass       0.44      0.07      0.12       709
             nummod       0.00      0.00      0.00         2
         nummod:gov       0.00      0.00      0.00       100
                obj       0.00      0.00      0.00        14
                obl       0.35      1.00      0.51      1987
             orphan       0.30      0.81      0.44       577
          parataxis       0.00      0.00      0.00        91
parataxis:discourse       0.00      0.00      0.00        35

        avg / total       0.16      0.33      0.18     10371

0.3338154469192942

'''
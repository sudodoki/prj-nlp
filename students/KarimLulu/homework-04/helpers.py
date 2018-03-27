from pathlib import Path

table = str.maketrans("\u2013", "-")

def _zip(items, label):
    return list(zip(items, [label]*len(items)))

def init_dir(folder):
    Path.mkdir(folder, parents=True, exist_ok=True)

def preprocess(s):
    return s.translate(table)

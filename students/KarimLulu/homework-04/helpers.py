from pathlib import Path
import re

table = str.maketrans("\u2013", "-")
patt_dates = r"\s*\(.*\)?"

def _zip(items, label):
    return list(zip(items, [label]*len(items)))

def init_dir(folder):
    Path.mkdir(folder, parents=True, exist_ok=True)

def preprocess(s):
    return s.translate(table)

def postprocess(s):
    return re.sub(patt_dates, "", s).strip().lower()

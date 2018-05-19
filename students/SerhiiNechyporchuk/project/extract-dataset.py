import sqlite3
import json
import csv

conn = sqlite3.connect('dataset/db')
c = conn.cursor()
rows = c.execute("select * from jobs").fetchall()

dicts = []
for row in rows:
    (id, author, created, modified, json_tasks, _) = row
    tasks = json.loads(json_tasks)
    for task in tasks:
        phrases = task['phrases']
        search_attrs = task['search-attributes']
        for phrase in phrases:
            if len(phrase) != 0:
                dicts.append({
                    'id': id,
                    'author': author,
                    'created': created,
                    'modified': modified,
                    'search_attrs': json.dumps(search_attrs),
                    'phrase': phrase})
dicts = list(sorted(dicts, key=lambda x: (x['modified'])))
last_modified = dicts[-1]['modified']

fname = 'dataset/phrases_{}.csv'.format(last_modified)
with open(fname, 'w') as wf:
    writer = csv.DictWriter(wf, ['id', 'author', 'created', 'modified', 'search_attrs', 'phrase'])
    writer.writeheader()
    writer.writerows(dicts)

print("Written into {}".format(fname))
# in this file we cross-check our dataset with wikipedia
# for creating "ground truth" list of albums with years,
# and then compare with albums created with retrieve_albums script
# and then print out all evaluation metrics

import requests
from bs4 import BeautifulSoup
import pandas as pd
import time
import re
import numpy as np
from collections import OrderedDict

from retrieve_albums import baseline, apply_rules

train_artists = ['The Smiths', 'R.E.M.', 'Talking Heads', 'Blondie', 'Pixies', 
           'Sonic Youth', 'Joy Division', 'Cocteau Twins',
           'My Bloody Valentine', 'The Stone Roses']

test_artists = ['The Cure', 'New Order', 'Primal Scream', 'Kraftwerk', 'The Cranberries', 
                'Weezer', 'Nine Inch Nails', 'Pavement', 'The Smashing Pumpkins', 
                'Built To Spill', 'Neutral Milk Hotel']

def get_studio_albums(artist):
    """
    Get list of studio albums from the discography section
    in the Wikipedia article
    """
    url = 'https://en.wikipedia.org/wiki/' + artist.replace(' ', '_')
    r = requests.get(url)
    soup = BeautifulSoup(r.content, 'html.parser')
    if 'may refer to' in soup.find_all('p')[0].get_text():
        url = 'https://en.wikipedia.org/wiki/' + artist.replace(' ', '_') + ' (band)'
        r = requests.get(url)
        soup = BeautifulSoup(r.content, 'html.parser')
    try:
        sa = soup.find('span', id='Discography')
        try:
            sas = sa.find_next(string='Studio albums')
        except:
            print('oops')
        ul = sas.find_next('ul')
        li = ul.find_all('li')
        albums = [re.sub(r'\([0-9]{4}.*', '', 
                         l.get_text().strip()).strip() for l in li]
    except:
        return None
    return albums

def construct_true_set(df, artist):
    """
    Use both our album DataFrame and Wikipedia
    to construct a set of albums and years
    for the specified artist.
    We check with Wiki because Discogs marks some
    non-studio albums (live, complations) like
    studiol albums
    """
    albums_df = df[df['artist']==artist][['title', 'year']]
    albums_from_wiki = [a.lower() for a in get_studio_albums(artist)]
    true_set = set(zip(albums_df['title'], albums_df['year']))
    true_set = set([(title, year) for (title, year) 
                    in true_set if title.lower() in albums_from_wiki])
    return true_set

def produce_wiki_text(artist):
    """
    Get text of a wiki page, no tables (<p> elements only)
    """
    url = 'https://en.wikipedia.org/wiki/' + artist.replace(' ', '_')
    r = requests.get(url)
    soup = BeautifulSoup(r.content, 'html.parser')
    wiki_text = '\n'.join(p.get_text() for p in soup.body.find_all('p'))
    return wiki_text

def construct_rule_set(func, artist):
    """
    Use our function for rule-based extraction to 
    construct a set of albums and years for the
    specified artist.
    """
    wiki_text = produce_wiki_text(artist)
    if len(wiki_text) < 100:
        wiki_text = produce_wiki_text(artist + ' (band)')
    return func(wiki_text)

def filter_duplicates(rule_set):
    """
    We might find the same album close to different years.
    Usually it is the earliest year that is the year of the release.
    """
    rule_list = sorted(rule_set, key = lambda x: x[1])
    rule_titles = [x[0] for x in rule_list]
    new_list = []
    for i in range(len(rule_titles)):
        if not rule_list[i][0] in rule_titles[:i]:
            new_list.append(rule_list[i])
    return set(new_list)

def custom_F(precision, recall, beta=1):
    """
    Custom F-metric, if we do not want to give
    equal weight to precision and recall
    """
    F = (1 + beta**2) * ((precision*recall)/(((beta**2)*precision) + recall))
    return F

def evaluate(true_set, rule_set, partial=False):
    """
    A function to provide assessment metrics for comparing 
    albums found by our function with albums from database
    """
    if len(rule_set) == 0:
        print('No albums are found for the rule set!')
        return None
    # we need to make sure that capitalization doesn't spoil anything!
    if partial:
        true_set = set([title.lower() for (title, year) in true_set])
        rule_set = set([title.lower() for (title, year) in rule_set])
    else:
        true_set = {(a[0].lower(), a[1]) for a in true_set}
        rule_set = {(a[0].lower(), a[1]) for a in rule_set}
        # filter full duplicates
        rule_set = filter_duplicates(set(rule_set))
    # count true positives, false positives, false negatives
    tp = true_set & rule_set
    fp = rule_set - true_set
    fn = true_set - rule_set
    # ...and our metrics
    precision = len(tp)/(len(tp)+len(fp))
    recall = len(tp)/(len(tp)+len(fn))
    if precision+recall == 0:
        print('Precision and recall are zero!')
        return None
    F1 = (2*precision*recall)/(precision + recall)
    F15 = custom_F(precision, recall, 1.5)
    stats = OrderedDict({
        'precision': round(precision, 3),
        'recall': round(recall, 3),
        'F1': round(F1, 3),
        'F1.5': round(F15, 3)
    })
    return stats

def count_average(stats):
    """
    A function to produce average results on a test set,
    given list of metric stats for each artist 
    """
    precisions = [d['precision'] for d in stats]
    recalls = [d['recall'] for d in stats]
    F1s = [d['F1'] for d in stats]
    F15s = [d['F1.5'] for d in stats]
    p = round(np.mean(precisions), 3)
    r = round(np.mean(recalls), 3)
    F1 = round(np.mean(F1s), 3)
    F15 = round(np.mean(F15s), 3)
    print(p, r, F1, F15)
    return p, r, F1, F15

train_album_df = pd.read_csv('train_albums.csv')
test_album_df = pd.read_csv('test_albums.csv')

baseline_stats_list = []
full_rule_stats_list = []
part_baseline_stats_list = []
part_rule_stats_list = []

for artist in test_artists:
    trueset = construct_true_set(test_album_df, artist)
    baseset = construct_rule_set(baseline, artist)
    ruleset = construct_rule_set(apply_rules, artist)
    time.sleep(0.2)
    print('------')
    print(artist)
    print('\nAlbums we need to retrieve:')
    print(trueset)
    print('Albums we have actually retrieved:')
    print(ruleset)
    print('>>>')
    print('metrics for baseline:')
    base_stats = evaluate(trueset, baseset)
    if base_stats:
        print(base_stats)
        baseline_stats_list.append(base_stats)
    print('metrics with rules:')
    rule_stats = evaluate(trueset, ruleset)
    if rule_stats:
        print(rule_stats)
        full_rule_stats_list.append(rule_stats)        
    print('------')
    print('Now with titles only:')
    print('metrics for baseline:')
    part_base_stats = evaluate(trueset, baseset, partial=True)
    if part_base_stats:
        print(part_base_stats)
        part_baseline_stats_list.append(part_base_stats)
    print('metrics for with rules:')
    prule_stats = evaluate(trueset, ruleset, partial=True)
    if prule_stats:
        print(prule_stats)
        part_rule_stats_list.append(prule_stats)
    print('------')
    
print('\nBaseline rule, full evaluation, average results:')
print('Average precision: {0}, average recall: {1}, average F1: {2}, average F15: {3}'.format(*count_average(baseline_stats_list)))

print('\nRule-based system, full evaluation, average results:')
print('Average precision: {0}, average recall: {1}, average F1: {2}, average F15: {3}'.format(*count_average(full_rule_stats_list)))

print('\nBaseline rule, partial evaluation, average results:')
print('Average precision: {0}, average recall: {1}, average F1: {2}, average F15: {3}'.format(*count_average(part_baseline_stats_list)))

print('\nRule-based system, partial evaluation, average results:')
print('Average precision: {0}, average recall: {1}, average F1: {2}, average F15: {3}'.format(*count_average(part_rule_stats_list)))
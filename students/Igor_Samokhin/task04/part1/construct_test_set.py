# in this script, we use Discogs API to get albums of artists,
# specified in test_artists variable, for our "get albums from wiki" program

import discogs_client
ds = discogs_client.Client('HW4Application/0.1', user_token='hvqMhwfUKKNEzUSUOJNKAhvsDSMtlAdLbEPgqPje')

import requests
from bs4 import BeautifulSoup
import pandas as pd
import time

album_df = pd.DataFrame(columns=['id', 'artist', 'title', 'type', 'year'])

test_artists = ['The Cure', 'New Order', 'Primal Scream', 'Kraftwerk', 'The Cranberries', 
                'Weezer', 'Nine Inch Nails', 'Pavement', 'The Smashing Pumpkins', 
                'Built To Spill', 'Neutral Milk Hotel']

for a in test_artists:
    print(a)
    results = ds.search(a, type="artist")
    time.sleep(60)
    artist = results[0]
    releases = artist.releases
    npages = releases.pages
    for i in range(npages):
        for r in releases.page(i+1):
            if not 'main_release' in r.data.keys():
                continue
            if not a.replace('The ', '') in r.data['artist']:
                continue
            rid = r.data['main_release']
            time.sleep(0.3)
            rel = ds.release(rid)
            reldict = {}
            reldict['artist'] = [artist.data['title']]
            reldict['id'] = [rid]
            reldict['title'] = [rel.title]
            if not 'descriptions' in rel.data['formats'][0].keys():
                continue
            reldict['type'] = ['|'.join(rel.data['formats'][0]['descriptions'])]
            reldict['year'] = [rel.year]
            rdf = pd.DataFrame(data = reldict)
            album_df = pd.concat([album_df, rdf])

album_df = album_df.reset_index()

# we filter out releases that are not studio albums
filter_out = 'Unofficial|Compilation|Remastered|Enhanced|Mini-Album'
test_df = album_df[(album_df['type'].str.contains('Album')) & ~((album_df['type'].str.contains(filter_out)))]
# and save the resulting dataset to csv
test_df.to_csv('testdf_albums.csv', index=False)


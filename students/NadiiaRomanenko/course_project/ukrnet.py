import requests
from time import sleep
from sqlalchemy import create_engine

with open('psql_credentials.txt') as f:
    engine = create_engine(f.read())

c = engine.connect()

headers = {
    'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0',
    'Cookie': '**ukr.net cookie**',
}

for _ in range(5):
    try:
        r = requests.get('https://www.ukr.net/news/dat/main/', headers=headers)
        topics = r.json()['tops']

        feed = []
        for topic in topics:
            topicid = topic['ClusterId']
            topic_created = topic['DateCreated']
            topic_last = topic['DateLast']
            if not 'News' in topic:
                feed += [{
                    'topic_id': topicid,
                    'topic_created': topic_created,
                    'topic_last': topic_last,
                    'new_created': topic['DateCreated'],
                    'new_id': topic['NewsId'],
                    'original_id': None,
                    'title': topic['Title'].replace("'", "''").replace("%", "%%"),
                    'link': topic['Url'],
                    'source': 'ukrnet',
                }]
            else:
                for n in topic['News']:
                    feed += [{
                        'topic_id': topicid,
                        'topic_created': topic_created,
                        'topic_last': topic_last,
                        'new_created': n['DateCreated'],
                        'new_id': n['Id'],
                        'original_id': None,
                        'title': n['Title'].replace("'", "''").replace("%", "%%"),
                        'link': n['Url'],
                        'source': 'ukrnet',
                    }]
                    if 'Dups' in n:
                        for d in n['Dups']:
                            feed += [{
                                'topic_id': topicid,
                                'topic_created': topic_created,
                                'topic_last': topic_last,
                                'new_created': d['DateCreated'],
                                'new_id': d['Id'],
                                'original_id': d['OriginalId'],
                                'title': d['Title'].replace("'", "''").replace("%", "%%"),
                                'link': d['Url'],
                                'source': 'ukrnet',
                            }]

        colnames = feed[0].keys()
        for news in feed:
            if news['original_id']:
                continue
            c.execute(f'''
                      INSERT INTO aggregators ({', '.join(colnames)})
                      VALUES ({', '.join(['%(' + cn + ')s' for cn in colnames])})
                      ON CONFLICT DO NOTHING;
                      ''', news)
        break

    except Exception as e:
        print(e)
        sleep(5)

c.close()
engine.dispose()

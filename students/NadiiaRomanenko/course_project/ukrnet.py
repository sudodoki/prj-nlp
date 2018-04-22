
import requests, psycopg2
from time import sleep

conn = psycopg2.connect(dbname='news',
                        user='nadiiaromanenko',
                        password='8t708y0oqwpEaldsFu8gt',
                        host='192.168.1.34',
                        port='5432')
c = conn.cursor()


# In[152]:


headers = {
    'User-Agent': 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0',
    'Cookie': 'uid=1CpM/FqesA4ZzQJ+CHiFAg==; un_lang=ua; un_news_region=9; can_https=1; tracknew=1520349199658005.1520444469.2; snr=9; scr=9; sfr=9; __utma=183793058.1645620879.1520349203.1520349203.1520444373.2; __utmz=183793058.1520349203.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); __utmv=183793058.|1=Users=Registered=1^2=Gender=f=1^3=Age=31=1; pcity=303010783; freemail=1520349384:BgyyS9hG:azarichanska@ukr.net:de2a68a05a85e1d0d5e8fd9a58232397:O2ezEAELCr5rj2i8; g=1; a=31; lang=uk; tmp=ZGVlaDNuZGVkaGZk; zakladki=0; __utmc=183793058; loginhesh=c677092a4c7abb8f3d25b7bd06b6f9d30ae20f63; __utmb=183793058.5.10.1520444373; __utmt=1; co=1',
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
                    'original_id': 'NULL',
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
                        'original_id': 'NULL',
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

        q_ins = f'''
        INSERT INTO aggregators ({', '.join(feed[0].keys())}) VALUES
        {', '.join(['(' + ', '.join(
                    ["'" + v + "'" if isinstance(v, str) and v != 'NULL' else str(v) for v in d.values()]) 
                    + ')' for d in feed])}
        ON CONFLICT DO NOTHING;
        '''
        c.execute(q_ins)
    except:
        sleep(5)

c.close()
conn.close()
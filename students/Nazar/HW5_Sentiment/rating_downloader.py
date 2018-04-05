
# coding: utf-8

# In[3]:


import requests


# In[6]:


import pandas as pd
clothes_links = pd.read_csv('gadjets_links.csv')


# In[25]:


links = []
import ast
for i in clothes_links.values:
    d=ast.literal_eval(i[0])
    links.append((d['comments_link'],d['name']))


# In[26]:


links[0]


# In[28]:


from lxml.html import soupparser
from scrapy import Selector
def parse_comments_url(data):
    url=data[0]
    name = data[1]
    print(name)
    header={}
    header['Connection']='keep-alive'
    header['Accept']='text/javascript, text/html, application/xml, text/xml, */*'
    header['X-Rozetka-Header']='true'
    header['X-Requested-With']='XMLHttpRequest'
    header['User-Agent']='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36'
    header['Accept-Encoding']='gzip, deflate, br'
    header['Accept-Language']='ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7'
    page = requests.get(url, headers=header)
    sel = Selector(_root=soupparser.fromstring(page.text))
    try:
        pages_count = int(sel.xpath("//li[@class='paginator-catalog-l-i']/span/text()").extract()[-1])
    except:
        pages_count=0
    data = parse_comments(sel, name)
    
    if pages_count>1:
        i=2
        while i<=pages_count:
            page = requests.get(url+'/page={0}'.format(i), headers=header)
            sel = Selector(_root=soupparser.fromstring(page.text))
            data = data + parse_comments(sel, name)
            i+=1
    return data

def parse_comments(response, name):
    vote_blocks = response.xpath("//div[@name='comment_vote']")
    data=[]
    for vote in vote_blocks:
        stars = vote.xpath(".//*[@class='g-rating-b']/span/span/@content").extract()
        #print(stars)
        texts = vote.xpath(".//div[@class='pp-review-text']/div/text()").extract()
        while(len(texts)<5):
            texts.append('')
        #print(texts
        star=-1
        if len(stars)>0:
            star=stars[0]
        data.append({'comment':texts[0], 'pros':texts[2],'cons':texts[4], 'stars': star,'name':name})
    return data

print(links[0])

from multiprocessing import Pool
import numpy as np
import pandas as pd

batch_size=1000
splits =  int(len(links)/batch_size)+1
print('--------------------------------------------------------------')
print(splits)
for i in range(8,splits+1):
    print(i)
    pool = Pool(16)
    res = pool.map(parse_comments_url , links[i*batch_size: (i+1)*batch_size])
    pool.close()
    data = np.concatenate(res)
    df = pd.DataFrame(list(data))
    df.to_csv('ratings_clothes_{0}.csv'.format(i), encoding='utf8', index=False)

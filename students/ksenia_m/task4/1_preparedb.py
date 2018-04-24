import urllib.request
import re

url = 'https://en.wikipedia.org/wiki/List_of_inventors'
response = urllib.request.urlopen(url)
data = response.read()
text = data.decode('utf-8')

items = re.findall('<li><a href="/wiki/.+" title="(.+)">.*-.*<a href=.+title="(.+)">.*</li>', text)
with open('train.db', 'a') as dbfile:
    for item in items:
    #<li><a href="/wiki/Nikolay_Zelinsky" title="Nikolay Zelinsky">Nikolay Zelinsky</a>
        if not 'List_of' in item[0]:
            dbfile.write('%s:%s\n' % item)

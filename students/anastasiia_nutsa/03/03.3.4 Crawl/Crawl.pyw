import requests
import warc
from urllib.parse import urlparse

def download_file():
    url = "https://commoncrawl.s3.amazonaws.com/crawl-data/CC-MAIN-2016-50/segments/1480698541905.26/warc/CC-MAIN-20161202170901-00429-ip-10-31-129-80.ec2.internal.warc.gz"
    fileName = url.split('/')[-1]
    print("Downloading...")
    request = requests.get(url, stream=True)
    with open(fileName, 'wb') as f:
        for chunk in request.iter_content(chunk_size=1024): 
            if chunk:
                f.write(chunk)
    return fileName

records = warc.open(download_file())
domains = {}
for record in records:
    if record.type == 'response':
        parsed = urlparse(record.url)
        domain = '{uri.netloc}'.format(uri=parsed)
        if domain not in domains:
            domains[domain] = 1
        else:
            domains[domain] += 1

lst = sorted(domains.items(), key = lambda x: x[1], reverse =True)

cnt_www = 0
cnt_com = 0
cnt_total = 0

for domain in domains:
    if domain.startswith("www"):
        cnt_www += 1
    if domain.endswith(".com"):
        cnt_com += 1
    cnt_total += 1    

print("\"www\"", 100 * (cnt_www/cnt_total))
print("\".com\" domains:", 100 * (cnt_com/cnt_total))
print("Popular:")
print(lst[0:10])

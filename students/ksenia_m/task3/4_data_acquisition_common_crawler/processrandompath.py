import random
import requests
import warc
import operator
from urllib.parse import urlparse

def download_file(url):
    local_filename = url.split('/')[-1]
    print("download from %s" % url)
    # NOTE the stream=True parameter
    r = requests.get(url, stream=True)
    with open(local_filename, 'wb') as f:
        for chunk in r.iter_content(chunk_size=1024): 
            if chunk: # filter out keep-alive new chunks
                f.write(chunk)
               #f.flush() commented by recommendation from J.F.Sebastian
    return local_filename

def random_file():
    random_path = random.choice(open("warc.paths").readlines())
    random_url = "https://commoncrawl.s3.amazonaws.com/%s" % random_path.replace("\n", "").replace("\r", "")
    return download(random_url)

def num_of_www_domains(domain_stats):
    counter=0
    for d in domain_stats:
        if d.startswith("www"):
            counter = counter + 1
    return counter

def num_of_dotcom_domains(domain_stats):
    counter=0
    for d in domain_stats:
        if d.endswith(".com"):
            counter = counter + 1
    return counter

f = warc.open(random_file())
domains_stat = {}
for record in f:
    if record.type == 'response':
        parsed_uri = urlparse(record.url)
        domain = '{uri.netloc}'.format(uri=parsed_uri)
        if domain not in domains_stat:
            domains_stat[domain]=1
        else:
            domains_stat[domain] = domains_stat[domain] + 1

sorted_stat = sorted(domains_stat.items(), key = operator.itemgetter(1), reverse =True)
print("Percent of domains starts with 'www'", 100 * (num_of_www_domains(domains_stat)/len(domains_stat)))
print("Percent of  .com ", 100 * (num_of_dotcom_domains(domains_stat)/len(domains_stat)))
print("Most popular domains:")
print(sorted_stat[0:10])

import re
import warc
from collections import defaultdict

# wget https://commoncrawl.s3.amazonaws.com/crawl-data/CC-MAIN-2018-13/segments/1521257644271.19/warc/CC-MAIN-20180317035630-20180317055630-00014.warc.gz

FILE = "CC-MAIN-20180317035630-20180317055630-00014.warc"


def leaders(xs, top=10):
    counts = defaultdict(int)
    for x in xs:
        counts[x] += 1
    return sorted(counts.items(), reverse=True, key=lambda tup: tup[1])[:top]



def analyze_warc_file():
    host = []
    server = []
    f = warc.WARCFile(FILE, "rb")
    for num, record in enumerate(f, start=1):
        line = None
        record.payload.readline()
        line = record.payload.readline()
        print(str(line))
        if 'Host' in str(line): host.append(re.search(r':(.*)', str(line)).group(1).strip().replace("\\r\\n'",''))
        if 'Server' in str(line): server.append(re.search(r':(.*)', str(line)).group(1).strip().replace("\\r\\n'", ''))
    print(server)
    print(host)
    return host, server

def statistic(host, server):
    print("TOP 10 hosts:")
    print(leaders(host))
    print("______________________________")
    print("TOP 10  servers:")
    total = len(server)
    s= leaders(server)
    for i in s:
        print('Server: {} percentage: {}'.format(i[0],(i[1]/total)*100))

host, server = analyze_warc_file()
statistic(host,server)

# OUTPUT:
#TOP 10 hosts:
#[('photos.smugmug.com', 143), ('www.target.com', 87),
# ('www.thefreedictionary.com', 55), ('www.shopcade.com', 50),
# ('encycolorpedia.jp', 47), ('www.worldcat.org', 46),
# ('weheartit.com', 44), ('animeonly.org', 41),
# ('www.supportforum.philips.com', 37), ('www.etsystudio.com', 37)]
#______________________________
#TOP 10  servers:
#Server: nginx percentage: 23.441305849420147
#Server: Apache percentage: 19.827680226851346
#Server: GSE percentage: 12.4731886428909
#Server: cloudflare percentage: 9.732068200821608
#Server: LiteSpeed percentage: 2.1158250627113095
#Server: Apache/2 percentage: 1.603228269167848
#Server: nginx/1.12.2 percentage: 1.5850510779074418
#Server: Apache/2.4.7 (Ubuntu) percentage: 1.07245428436398
#Server: nginx/1.12.1 percentage: 0.996110081070273
#Server: Apache-Coyote/1.1 percentage: 0.9415785072890537

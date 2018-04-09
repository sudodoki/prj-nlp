import re
import json
import warc
from collections import defaultdict
from urlparse import urlparse
reg = re.compile('Server: .[A-Z,a-z]+')
domains = defaultdict(int)
servers = defaultdict(int)


def smart_round_float(num, precision=1):
    """
    taken from somewhere else
    round numbers in more human readable format
    e.g.
    341.123434 mUSD -> 341mUSD
    0.45345345 mUSD -> 0.5mUSD
    0.034545 mUSD -> 0.03mUSD
    0.0014545 mUSD -> 0.001mUSD
    0.000000005 mUSD -> 5e10-8 mUSD
    etc.

    precision defines how many numbers to round to.

    Return a number (int or float).
    """
    if num < 0.0001:
        # this is to work with scientific notation; :f (:6) means 6 digits after comma
        formatter = "{" + ":.{}g".format(precision+1) + "}"
        return float(formatter.format(num))
    str_num = str(num)
    if '.' in str_num:
        whole_part, dec_part = str_num.split(".")
        whole_part_len = len(whole_part)
        if whole_part_len > 2:
            return int(num)
        else:
            try:
                round_to = re.search(r'[^0]', dec_part).start()
                return round(num, round_to + precision)
            except AttributeError:
                return int(num)
    else:
        return num

def analyze_warc_file():
    f = warc.WARCFile("CC-MAIN-20180221103712-20180221123712-00305.warc", "rb")
    for num, record in enumerate(f, start=1):
        line = None
        if record.type == "response":
            domains[urlparse(record.url)[1]] += 1
            record.payload.readline()
            line = record.payload.readline() #always second line
            if line:
                res = reg.search(line)
                if res:
                    server = res.group().split("Server: ")[1]
                    servers[server]+=1

    print("TOP 10 domains:")
    for k in sorted(domains, key=domains.__getitem__, reverse=True)[:10]:
        print("{}: {} records".format(k ,smart_round_float(domains[k])))
    print("=======")
    print("Servers:")
    total = sum(servers.values())
    for k in sorted(servers, key=servers.__getitem__, reverse=True)[:10]:
        print("{}: {}%".format(k ,smart_round_float(servers[k]*100.0/total)))
# OUTPUT:
# TOP 10 domains:
# businessfinder.nj.com: 58 records
# photos.smugmug.com: 48 records
# homes.kw.com: 48 records
# www.mynewsdesk.com: 40 records
# www.maxbimmer.com: 36 records
# www.hs.fi: 36 records
# gazeta-rvs.ru: 35 records
# www.play-asia.com: 34 records
# farm3.staticflickr.com: 34 records
# www.culture.ru: 33 records
# =======
# Servers:
# nginx: 37.9%
# Apache: 31.1%
# GSE: 13.4%
# cloudflare: 9.3%
# LiteSpeed: 1.8%
# Microsoft: 1.5%
# openresty: 1.2%
# mw: 0.3%
# ATS: 0.2%
# Tengine: 0.2%

if __name__ == "__main__":
    analyze_warc_file()
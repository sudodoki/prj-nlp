import re
import json

from collections import defaultdict
from urllib.parse import urlsplit
reg = re.compile('{"url": ".*"')
domains = defaultdict(int)
status_codes = defaultdict(int)

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

def analyze():
    with open("cdx-00204", "r") as fl:

        while True:
            line = fl.readline()
            if not line:
                break
            info = json.loads('{"url":' + line.split('{"url":')[1].strip())
            domain = urlsplit(info['url'])[1].split(".")[-1].split(":")[0]
            domains[domain] += 1
            status_codes[info['status']] += 1
    total = sum(domains.values())
    print("TOP 10 domains:")
    for k in sorted(domains, key=domains.__getitem__, reverse=True)[:10]:
        print("{}: {}%".format(k ,smart_round_float(domains[k]*100/total)))
    print("=======")
    print("Response codes:")
    total = sum(status_codes.values())
    for k in sorted(status_codes, key=status_codes.__getitem__, reverse=True):
        print("{}: {}%".format(k ,smart_round_float(status_codes[k]*100/total)))
#OUTPUT:
# TOP 10 domains:
# hu: 44.1%
# gr: 17.7%
# hr: 17.2%
# hk: 13.8%
# guru: 1.4%
# gt: 1.2%
# hn: 1.04%
# gs: 1.0%
# ht: 0.4%
# host: 0.3%
# =======
# Response codes:
# 200: 84.3%
# 301: 6.0%
# 302: 5.0%
# 404: 2.7%
# 503: 0.6%
# 403: 0.5%
# 500: 0.3%
# 511: 0.1%
# 303: 0.1%
# 429: 0.1%
# 410: 0.09%
# 430: 0.08%
# 502: 0.04%
# 401: 0.02%
# 400: 0.02%
# 307: 0.01%
# 520: 0.005%
# 508: 0.003%
# 521: 0.002%
# 504: 0.002%
# 406: 0.001%
# 204: 0.001%
# 409: 0.001%
# 509: 0.001%
# 408: 0.0007%
# 523: 0.0006%
# 423: 0.0006%
# 414: 0.0005%
# 526: 0.0004%
# 402: 0.0002%
# 405: 0.0001%
# 490: 0.0001%
# 415: 9.8e-05%
# 416: 8.6e-05%
# 456: 6.1e-05%
# 525: 5.5e-05%
# 422: 4.9e-05%
# 530: 4.3e-05%
# 601: 3.7e-05%
# 300: 3.7e-05%
# 475: 3.7e-05%
# 418: 3.1e-05%
# 501: 2.5e-05%
# 1062: 1.2e-05%
# 0: 1.2e-05%
# 552: 1.2e-05%
# 412: 6.1e-06%
# 533: 6.1e-06%
# 999: 6.1e-06%



if __name__ == "__main__":
    analyze()
Server: Apache\r\n
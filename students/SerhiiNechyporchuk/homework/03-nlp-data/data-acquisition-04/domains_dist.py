import warc
from urllib.parse import urlparse
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import collections
import argparse

argparser = argparse.ArgumentParser(description='processes warc files and returns pie chart with domains distribtuion')
argparser.add_argument('--input', required=True, help='Path to the warc file')
argparser.add_argument('--output', required=True, help='Path to the output image of pie chart')
args = argparser.parse_args()

domains_counter = collections.Counter()
with warc.open(args.input) as f:
    for record in f:
        if 'WARC-Target-URI' not in record:
            continue
        else:
            host_with_port = urlparse(record['WARC-Target-URI']).netloc
            host = host_with_port.split(':')[0]
            domain = host.split('.')[-1]
            domains_counter.update([domain])

common = dict(domains_counter.most_common(10))
domains = list(common.keys())
total = sum(common.values())
shares = [common[domain]/total for domain in domains]

fig, ax = plt.subplots()
patches, texts = ax.pie(shares, shadow=True, radius=1)
ax.axis('equal')
plt.legend(patches, domains, loc='upper left')
plt.savefig(args.output)

print('Pie chart is saved at {}'.format(args.output))
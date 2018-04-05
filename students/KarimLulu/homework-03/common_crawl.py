from pathlib import Path
import urllib.parse
from collections import Counter
import warc
import gzip
import json

from config import data_dir, hosts_output, servers_output

PATT = "*wat*"

def process_files(patt=PATT):
    files = list(data_dir.glob(patt))
    host_counter, server_counter = Counter(), Counter()
    for file in files:
        ccfile = warc.WARCFile(fileobj=gzip.open(file))
        for i, record in enumerate(ccfile):
            if record['Content-Type'] != 'application/json':
                continue

            payload = record.payload.read()
            data = json.loads(payload)

            if data['Envelope']['WARC-Header-Metadata']['WARC-Type'] != 'response':
                continue

            url = data["Envelope"]["WARC-Header-Metadata"].get("WARC-Target-URI")
            if url:
                host = urllib.parse.urlparse(url).netloc.lower()
                host_counter.update([host])

            server = data['Envelope']['Payload-Metadata']['HTTP-Response-Metadata']['Headers'].get('Server')
            if server:
                server_counter.update([server])
    return host_counter, server_counter

def save_counter(counter, filename, folder=data_dir):
    with (folder / filename).open("w+") as f:
        for key,value in counter.most_common():
            f.write(f"{key}: {value}\n")

if __name__ == "__main__":
    host_counter, server_counter = process_files()
    save_counter(host_counter, filename=hosts_output)
    save_counter(server_counter, filename=servers_output)

import random
import re
import requests
import warc
import operator
from urllib.parse import urlparse
from bs4 import BeautifulSoup
from langdetect import detect
from langdetect.lang_detect_exception import LangDetectException


def download_file(url):
    local_filename = url.split('/')[-1]
    print("download from %s" % url)
    r = requests.get(url, stream=True)
    with open(local_filename, 'wb') as f:
        for chunk in r.iter_content(chunk_size=1024): 
            if chunk: # filter out keep-alive new chunks
                f.write(chunk)
    return local_filename


def random_file():
    # random_path = random.choice(open("warc.paths").readlines())
    # random_url = "https://commoncrawl.s3.amazonaws.com/%s" % random_path.replace("\n", "").replace("\r", "")
    # return download_file(random_url)
    return "CC-MAIN-20160428161523-00082-ip-10-239-7-51.ec2.internal.warc.gz"


def num_of_www_domains(domain_stats):
    counter = 0
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

def plain_text(html):
    if not html:
        return None
    # remove these tags, complete with contents.
    blacklist = ["script", "style"]

    whitelist = [
        "div", "span", "p", "br", "pre",
        "table", "tbody", "thead", "tr", "td", "a",
        "blockquote",
        "ul", "li", "ol",
        "b", "em", "i", "strong", "u", "font"]

    body = BeautifulSoup(html, "html.parser").find("body")
    if not body:
        return None

    # now strip HTML we don't like.
    for tag in body.findAll():
        if tag.name.lower() in blacklist:
            tag.extract()
        elif not tag.name.lower() in whitelist:
            tag.name = "span"
            tag.attrs = []

    return body.get_text()


f = warc.open(random_file())
domains_stat = {}
language_stat = {}
num_undetect_lang = 0
counter = 0
for record in f:
    if record.type == 'response':
            #and record.http_headers.get_header('Content-Type') == 'text/html':
        counter += 1
        content = record.payload.read()
        text = plain_text(content)
        if text:
            try:
                lang = detect(text)
                if lang not in language_stat:
                    language_stat[lang] = 1
                else:
                    language_stat[lang] = language_stat[lang] + 1
            except LangDetectException:
                num_undetect_lang += 1
                #print("%%%%" + record.url)
            #print(soup.find("body").get_text())

            parsed_uri = urlparse(record.url)
            domain = '{uri.netloc}'.format(uri=parsed_uri)
            if domain not in domains_stat:
                domains_stat[domain]=1
            else:
                domains_stat[domain] = domains_stat[domain] + 1

sorted_stat = sorted(domains_stat.items(), key = operator.itemgetter(1), reverse =True)
sorted_lang = sorted(language_stat.items(), key = operator.itemgetter(1), reverse =True)
print("Total num of pages: ", counter)
print("Percent of domains starts with 'www'", 100 * (num_of_www_domains(domains_stat)/len(domains_stat)))
print("Percent of  .com ", 100 * (num_of_dotcom_domains(domains_stat)/len(domains_stat)))
print("Most popular domains:")
print(sorted_stat[0:10])
print("Most popular langauges:")
print(sorted_lang[0:20])
print("Number of domains with undetected language:", num_undetect_lang)

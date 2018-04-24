import requests
import pandas as pd
from bs4 import BeautifulSoup
import time
import re

# start by scraping a kitchen section of rozetka
r = requests.get('https://bt.rozetka.com.ua/ua/tehnika-dlya-kuhni/c435974/')
section = BeautifulSoup(r.content, 'html.parser')

# find urls for each category in the section 
cat_urls = [a.get('href').replace('http://', 'https://') 
            for a in section.find_all('a', class_='m-cat-subl-i-link')]
all_reviews = []

# for each category, find each product and all reviews for the product
# works for quite a long time

for cat_url in cat_urls[30:]:
    if len(requests.get(cat_url+'filter/').history) > 0:
        cat_r = requests.get(cat_url)
    else:
        cat_url += 'filter/'
        cat_r = requests.get(cat_url)
    cat = BeautifulSoup(cat_r.content, 'html.parser')
    print(cat.title.get_text())
    products = cat.find_all('div', id=re.compile(r'^catalog_item.*'))
    n = 2
    while True:
        page_url = cat_url+'page={n}/'.format(n=n)
        r_page = requests.get(page_url, allow_redirects=False)
        if r_page.status_code != 200:
            break
        if 'location' in r_page.headers:
            new_url = r_page.headers['Location']
            r_page = requests.get(new_url+'page={n}/'.format(n=n))            
        cat_page = BeautifulSoup(r_page.content, 'html.parser')
        print(cat_page.title.get_text())
        products.extend(cat_page.find_all('div', id=re.compile(r'^catalog_item.*')))
        n += 1
        time.sleep(0.5)
    print('How many products in a category we found:', len(products))
    for prod in products:
        if not prod:
            continue
        n_rev_cont = prod.find('div', class_='g-rating')
        if not n_rev_cont:
            continue
        n_reviews = n_rev_cont.find('a').get('data-count')
        if not n_reviews:
            continue
        reviews_url = n_rev_cont.find('a').get('href')
        if not reviews_url:
            continue
        reviews_r = requests.get(reviews_url)
        time.sleep(0.5)
        rev = BeautifulSoup(reviews_r.content, 'html.parser')
        reviews = rev.find_all('article', class_='pp-review-i')
        n = 2
        while True:
            rev_page_url = reviews_url+'page={n}/'.format(n=n)
            r_rev_page = requests.get(rev_page_url, allow_redirects=False)
            if r_rev_page.status_code != 200:
                break
            rev_page = BeautifulSoup(r_rev_page.content, 'html.parser')
            if not rev_page.find_all('article', class_='pp-review-i'):
                break
            reviews.extend(rev_page.find_all('article', class_='pp-review-i'))
            n += 1
            time.sleep(0.5)
        for review in reviews:
            rating_cont = review.find('meta', {'itemprop': 'ratingValue'})
            if not rating_cont:
                continue
            rating = rating_cont.get('content')
            review_text = review.find('div', class_='pp-review-text').get_text()
            all_reviews.append((int(rating), review_text))

# make a pandas dataframe from all of this
df = pd.DataFrame(all_reviews)
df = df.rename(columns = {'0': 'rating', '1': 'raw_text'})

from langdetect import detect

# clean text of words "Переваги:" and "Недоліки:"
def clean_text(text):
    text = (text.strip()
            .replace('\n\n\nПереваги:\xa0', '')
            .replace('\n\n\nНедоліки:\xa0', ''))
    return text
df['text'] = df['raw_text'].apply(clean_text)

# detect language
def detect_lang(text):
    try:
        res = detect(text)
    except:
        res = 'na'
    return res

df['lang'] = df['text'].apply(detect_lang)

# variable to check if there are advantages and disadvantages in text
def check_adv(text):
    if '\n\n\nПереваги:\xa0' in text:
        if '\n\n\nНедоліки:\xa0' in text:
            return 'both'
        else:
            return 'adv_only'
    elif '\n\n\nНедоліки:\xa0' in text:
        return 'disadv_only'
    else:
        return 'neither'
    
df['is_adv'] = df['raw_text'].apply(check_adv)

# extract text without list of advantages and disadvantages
def clean_adv(row):
    text = row['raw_text']
    is_adv = row['is_adv']
    pat1 = r'(.*?)\n\n\nПереваги:\xa0.*'
    pat2 = r'(.*?)\n\n\nНедоліки:\xa0.*'
    if (is_adv == 'both') or (is_adv == 'adv_only'):
        return regex.search(pat1, text).captures(1)[0]
    elif is_adv == 'disadv_only':
        return regex.search(pat2, text).captures(1)[0]
    return text.strip()

df['without_adv'] = df.apply(clean_adv, axis=1)

# extract separately text for advantages and for disadvantages
def get_adv_disadv(text, is_adv):
    pat1 = r'(?:.*?)\n\n\nПереваги:\xa0(.*?)\n\n\nНедоліки:\xa0(.*)'
    pat2 = r'(?:.*?)\n\n\nПереваги:\xa0(.*)'
    pat3 = r'(?:.*?)\n\n\nНедоліки:\xa0(.*)'
    if is_adv == 'both':
        match = regex.search(pat1, text, re.DOTALL)
        adv, disadv = match.captures(1)[0], match.captures(2)[0]
    elif is_adv == 'adv_only':
        adv = regex.search(pat2, text, re.DOTALL).captures(1)[0]
        disadv = ''
    elif is_adv == 'disadv_only':
        adv = ''
        disadv = regex.search(pat3, text, re.DOTALL).captures(1)[0]
    else:
        adv, disadv = '', ''
    return adv.strip(), disadv.strip()

def extract_adv(row):
    text = row['raw_text']
    is_adv = row['is_adv']
    return get_adv_disadv(text, is_adv)[0]

def extract_disadv(row):
    text = row['raw_text']
    is_adv = row['is_adv']
    return get_adv_disadv(text, is_adv)[1]

df['adv'] = df.apply(extract_adv, axis=1)
df['disadv'] = df.apply(extract_disadv, axis=1)

# save all data
df = df[['rating', 'raw_text', 'text', 'lang', 'without_adv', 'adv', 'disadv']]
df.to_csv('all_reviews.csv', index=False)

# save Ukrainian reviews only
df_uk = df[df['lang']=='uk']
df_uk.to_csv('ukr_reviews.csv', index=False)
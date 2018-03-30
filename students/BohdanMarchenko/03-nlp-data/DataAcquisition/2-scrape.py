import os

import requests
from bs4 import BeautifulSoup

SITE = "http://forum.lvivport.com/"
ROOT_DIR = "scrape_result"

def get_data(url, counter, cls):
    url = url.format(SITE, counter)
    response = requests.get(url)

    if response.status_code != 200:
        print('END: request status:', response.status)
        return

    if "/page-" in response.url:
        curr_page = int(response.url.split("/page-")[1])
    else:
        curr_page = 1
    if curr_page != counter:
        print('END: final page reached:', counter-1)
        return
    soup = BeautifulSoup(response.text, 'html5lib')
    for script in soup(["script", "style"]):
        script.decompose()  # rip it out
    data = soup.select(cls)
    if not data:
        print('END: no data:')
        return
    return data

def scrape():
    counter = 1
    if not os.path.exists(ROOT_DIR):
        os.mkdir(ROOT_DIR)
    while True:
        categories_data = get_data("{}forums/vsjake-rizne.46/page-{}", counter, '.PreviewTooltip')
        if not categories_data:
            break
        counter += 1
        for category in categories_data:
            print("processing {}".format(category.text))
            inner_counter = 1
            href = category.attrs['href']
            dirpath = "{}/{}".format(ROOT_DIR, category.text.replace("/", ""))
            if not os.path.exists(dirpath):
                os.mkdir(dirpath)
            while True:
                print("getting page #{}".format(inner_counter))
                data = get_data("{}"+href+"page-{}", inner_counter, ".messageList li.message")
                print("{}{}page-{}".format(SITE, href, inner_counter))

                inner_counter += 1
                if not data:
                    break
                for i in data:
                    if "id" in i.attrs:
                        file = open("{}/{}.txt".format(dirpath, i.attrs["id"]), "w")
                        username = i.find('a', {"class": "username"}).text
                        date = i.find('span', {"class": "DateTime"})
                        if not date:
                            date = i.find('abbr', {"class": "DateTime"})
                        permalink = i.find('a', {"class": "hashPermalink"}).attrs["href"]
                        text = i.find("article").text.replace("р е к л а м а", "").strip()
                        file.write("\n".join((username, date.text, permalink, text)))
                        file.close()
    print("DONE: processed {} pages".format(counter))


if __name__ == "__main__":
    scrape()

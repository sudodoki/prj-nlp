from urllib.request import urlopen
from bs4 import BeautifulSoup


def get_comment(main_page="https://rozetka.com.ua/asus_vivobook_max_x541na_go123/p17982198/#tab=comments/"):
    all_forums = []
    html = urlopen(main_page)
    bs_obj = BeautifulSoup(html, "lxml")
    for node_obj in bs_obj.findAll("li", {"name": "comments"}):
        node_info = node_obj.find('a').attrs.get('href')
        # node_info = node_obj.find('div', {'class': 'g-rating-b'})  # .find('a')
        # node_link = node_info.attrs.get('href')
        # node_text = node_info.getText()
        all_forums.append(node_info)
    return all_forums


f = get_comment()
print(f)

import re
import random
from datetime import datetime
from urllib.request import urlopen
from bs4 import BeautifulSoup


LVIV_FORUM_ADDR = "http://forum.lvivport.com/"
UKR_MONTHS = {'січ': 1, 'лют': 2, 'бер': 3, 'кві': 4, 'тра': 5, 'чер': 6,
              'лип': 7, 'сер': 8, 'вер': 9, 'жов': 10, 'лис': 11, 'гру': 12}


class Post(object):
    """
    Describes post
    Attributes:
        post_id (str): id
        message (str): text of the post
        author (str): author name
        datetime (datetime): date and time (hh:mm) of the post
    """
    def __init__(self, post_id, message, author, datetime):
        self.post_id = post_id
        self.message = message.strip()
        self.author = author
        self.datetime = datetime

    def __str__(self):
        return 'date: {}\nauthor: {}\n{}\n'.format(self.datetime, self.author, self.message)


class Thread(object):
    """
    Describes thread entity
    Attributes:
        thread_id (str): id of the thread
        link (str): link of the thread
        name (str): name of the thread
        pages_num (int): number of pages with posts
        posts (list): list of post
    """
    def __init__(self, thread_id, link, name, pages_num):
        self.thread_id = thread_id
        self.link = link
        self.name = name
        self.pages_num = pages_num
        self.posts = []

    def collect_all_posts(self):
        """
        extract all posts from all pages of the thread
        """
        self.posts += self.get_posts_on_page(self.link)
        if self.pages_num > 1:
            for n in range(2, self.pages_num+1):
                self.posts += self.get_posts_on_page('{}page-{}'.format(self.link, n))

    def get_posts_on_page(self, link):
        """
        extract all posts from the page
        :param link: link to the page with posts
        :return: list of post objects
        """
        posts = []
        html = urlopen(link)
        bs_obj = BeautifulSoup(html, "lxml")
        for post_obj in bs_obj.findAll("li", {"id": re.compile('post-')}):
            post_id = int(re.findall(r'\d+', post_obj.attrs.get('id', '0'))[0])
            post_text = post_obj.find('blockquote').getText()
            post_text = self.extract_text(post_text)  # clean the message
            author_info = post_obj.find('a', {'class': 'username author'})
            author_name = author_info.getText()
            post_date = post_obj.find('span', {'class': 'DateTime'}).attrs.get('title', '')
            post_date = self.extract_date(post_date)  # prepare date object
            posts.append(Post(post_id, post_text, author_name, post_date))
        return posts

    @staticmethod
    def extract_text(text):
        """
        Prepare text and remove useless info
        :param text: text
        :return: text after cleaning
        """
        text = re.sub(r'\n', ' ', text)
        text = re.sub(r'\<!--.*--\>', '', text)
        text = re.sub(r'\sр е к л а м а\s', '', text)
        text = re.sub(r'\s+', ' ', text)
        return text

    @staticmethod
    def extract_date(text):
        """
        get date of the post
        :param text: text with date
        :return: dae object
        """
        date = re.search(r'(?P<day>\d{1,2}) (?P<month>\w+) (?P<year>\d{4}) у (?P<hour>\d{1,2}):(?P<min>\d{1,2})', text)
        month = UKR_MONTHS.get(date.group('month'), 1)
        return datetime(int(date.group('year')), month, int(date.group('day')),
                        int(date.group('hour')), int(date.group('min')))

    def print_posts(self, n=5):
        """
        print first n posts of the thread
        :param n: number of first posts, default = 5
        """
        [print(x) for x in self.posts[:n]]

    def __str__(self):
        return 'id: {}\nlink: {}\nname: {}\npages: {}\n'.format(self.thread_id, self.link, self.name, self.pages_num)

    def __eq__(self, other):
        return self.link == other.link


class Forum(object):
    """
    Describe forum entity
    Attributes:
        forum_link (str): link to forum
        forum_name (str): forum name
        threads (list): list of threads in the forum
    """
    def __init__(self, link, name):
        self.forum_link = link
        self.forum_name = name
        self.threads = []

    def extract_threads(self):
        """
        get all threads from all pages of the forum
        """
        html = urlopen(self.forum_link)
        bs_obj = BeautifulSoup(html, "lxml")
        self.threads = self.get_threads_on_page(self.forum_link)
        pages_num = self.get_max_page(bs_obj)
        for n in range(2, pages_num + 1):
            self.threads += self.get_threads_on_page('{}page-{}'.format(self.forum_link, n))

    @staticmethod
    def get_threads_on_page(link):
        """
        get threads from the current page
        :param link: link tho the page
        :return: list of threads objects on the page
        """
        temp_threads = []
        html = urlopen(link)
        bs_obj = BeautifulSoup(html, "lxml")
        for node_obj in bs_obj.findAll("li", {"id": re.compile('thread-')}):
            node_id = int(re.findall(r'\d+', node_obj.attrs.get('id', '0'))[0])
            node_info = node_obj.find('div', {'class': 'titleText'})
            node_link = node_info.find('a', {'class': 'PreviewTooltip'}).attrs.get('href')
            node_link = '{}{}'.format(LVIV_FORUM_ADDR, node_link)
            node_text = node_info.find('a', {'class': 'PreviewTooltip'}).getText()
            pages_num = 1
            pages = node_info.find('span', {'class': 'itemPageNav'})
            if pages:
                pages_num = max([int(x.getText()) for x in pages.findAll('a')])
            temp_threads.append(Thread(node_id, node_link, node_text, pages_num))
        return temp_threads

    @staticmethod
    def get_max_page(bs_obj):
        """
        get number of pages with the threads
        :param bs_obj: bs object
        :return: number of the pages
        """
        pages = bs_obj.find("div", {"class": "PageNav"})
        if pages:
            return int(pages.attrs.get('data-last', 1))
        return 1

    def get_threads(self):
        """
        get threads of the forum
        :return: list of the threads
        """
        return self.threads


def get_forums(main_page="http://forum.lvivport.com/"):
    all_forums = []
    html = urlopen(main_page)
    bs_obj = BeautifulSoup(html, "lxml")
    for node_obj in bs_obj.findAll("li", {"class": re.compile('node forum level_2')}):
        node_info = node_obj.find('h3', {'class': 'nodeTitle'}).find('a')
        node_link = node_info.attrs.get('href')
        node_text = node_info.getText()
        all_forums.append(Forum('{}{}'.format(main_page, node_link), node_text))
    return all_forums


# collect all possible forum
forums = get_forums()
# get 1 random forum to collect threads
random_forum = random.choice(forums)
# get all threads from the forum
random_forum.extract_threads()
# get random thread
random_thread = random.choice(random_forum.get_threads())
random_thread.collect_all_posts()
print(random_thread)
print(random_thread.print_posts(10))

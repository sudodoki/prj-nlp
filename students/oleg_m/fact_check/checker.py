import re
import csv
import spacy
import requests
import numpy as np
import pandas as pd
import wikipedia
from wikipedia.exceptions import WikipediaException
from fuzzywuzzy import fuzz, process

section_words = {'bibliography', 'works'}
nlp = spacy.load('en_core_web_sm')


def compare_names(book_title, books_set):
    """
    is the book is present in the scrapped data
    :param book_title: current book title
    :param books_set: set of scraped books of the author
    :return: is the book in the scrapped data
    """
    score = process.extractOne(book_title, books_set, scorer=fuzz.token_set_ratio)
    if books_set and score[1] > 80:
        return True
    return False


def get_from_tables(link):
    """
    parse tables from the wiki page
    :param link: link of the page
    :return: set of books
    """
    books = set()
    response = requests.get(link)
    df_list = pd.read_html(response.content, header=0)
    for df in df_list:
        title_column = None
        for col in df.columns:
            # find the column name named like title
            score = fuzz.token_set_ratio('title', col)
            if score > 80:
                title_column = col
                break
        if title_column:
            books = books.union(set(df[title_column].tolist()))
    return books


def compare_common_page(page, title):
    """
    parses the page of the author and compare books title to the scraped data
    :param page: wiki page
    :param title: title of the book
    :return: is the book is written by the author
    """
    lines = page.content.split('\n')
    # flag if the line is bibliography section
    is_bibl = False
    # number of separator symbols of bibliography section
    count_separator = 0
    books = set([])
    # try to scrap from the text
    for line in lines:
        if not is_bibl:
            if re.search(r'=+.*{}.*=+'.format('|'.join(section_words)), line, re.IGNORECASE):
                header = re.search('=+', line)
                count_separator = header.end() - header.start()
                is_bibl = True
        elif is_bibl:
            if re.search('=+', line):
                header = re.search('=+', line)
                if header.end() - header.start() <= count_separator:
                    break
            doc = nlp(line)
            books = books.union(set([str(x).strip(' (),."\'').lower() for x in doc.ents
                                     if x.label_ in {'ORG', 'WORK_OF_ART'}]))
    print('books:', books)
    return compare_names(title, books)


def compare_bibliography_page(page, title):
    """
    parses the page of the author bibliography and compare books title to the scraped data
    :param page: wiki page
    :param title: title of the book
    :return: is the book is written by the author
    """
    lines = page.content.split('\n')
    is_bibl = False
    books = set([])
    for line in lines:
        if not is_bibl:
            if '==' in line:
                is_bibl = True
        else:
            if '==' not in line:
                doc = nlp(line)
                books = books.union(set([str(x).strip(' (),."\'').lower() for x in doc.ents
                                         if x.label_ in {'ORG', 'WORK_OF_ART'}]))
    print('books:', books)
    # if no text was found, try to get tables
    if not books:
        books = get_from_tables(page.url)
        print('books:', books)
    return compare_names(title, books)


def check_book(author, book_title, author_pages, author_bibl_pages):
    """
    checks is the author wrote the book from wiki
    :param author: author name from the db
    :param book_title: book's name from the db
    :param author_pages: found pages of the author in wiki
    :param author_bibl_pages: found pages of the author's bibliography in wiki
    :return: (bool) of the book was written by the author
    """
    # assumption if search result of author is the book, the book is author's
    for pages in author_pages:
        if fuzz.token_set_ratio(book_title, pages) > 80:
            return True
    # if writer does not have bibliography page
    if author_pages[0] == author_bibl_pages[0]:
        page = wikipedia.page(author_pages[0])
        is_authors = compare_common_page(page, book_title)
    # if author has the bibliography page
    elif '{} {}'.format(author, 'bibliography') == author_bibl_pages[0]:
        page = wikipedia.page(author_bibl_pages[0])
        is_authors = compare_bibliography_page(page, book_title)
    # if author has the bibliography page but slightly different name
    elif 'bibliography' in author_bibl_pages[0] and fuzz.token_set_ratio(author,
                                                                         author_bibl_pages[0],
                                                                         force_ascii=True) > 60:
        page = wikipedia.page(author_bibl_pages[0])
        is_authors = compare_bibliography_page(page, book_title)
    else:
        page = wikipedia.page(author_pages[0])
        is_authors = compare_common_page(page, book_title)

    return is_authors


def test_file(filepath):
    # with open('files/test_books.csv', newline='') as test_file:
    with open(filepath, newline='') as test_file:
        reader = csv.reader(test_file)
        next(reader)
        error_checks = []
        for row in reader:
            print(row[:2])
            title = re.sub('\(.+\)', '', row[0])
            try:
                is_book = compare_book(row[1], title)
                print(is_book)
            except WikipediaException as e:
                print('{} - {} missed. {}'.format(row[1], title, e))
                error_checks.append((row[1], title))

            print()
            break


def compare_book(author, book_title):
    """
    get wiki pages of the author
    :param author: author name
    :param book_title: book's name
    :return: if the algorithm finds the author in wiki, returns is he wrote a book, else returns nan
    """
    results = wikipedia.search(author, results=5)
    results_bibl = wikipedia.search('{} {}'.format(author, 'bibliography'), results=5)
    print('{} - {}'.format(author, book_title))
    author_score = fuzz.token_set_ratio(author, results[0], force_ascii=True)
    if author_score > 80:
        try:
            return check_book(author, book_title, results, results_bibl)
        except WikipediaException as e:
            print('{} - {} missed. {}'.format(author, book_title, e))
    return np.nan




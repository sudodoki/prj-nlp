## Task 4

+ Language: Python3
+ Required external libraries: numpy, pandas, spacy, sklearn, wikipedia, fuzzywuzzy
+ Script path: students/oleg_m/02-data/1-2_annts.py
+ External file:
    + files/books.zip - dirty db of books and authors
    + files/test_books.csv - 10 books and their correct authors with and 10 books with fake authors
+ Run command: python3 students/oleg_m/fact_check/evaluate.py

### Database

The file was found in Internet with the books and their authors and category. Only fiction, novels, science Fiction & fantasy were selected.
The titles may contains some littered data (E.g. The Three Musketeers (Word Cloud Classics)

### Baseline

Algorithm get the author name and searches related pages in Wikipedia using wikipedia Python lib.
The results' page may be: author wiki page, author bibliography page, author's related pages, pages of author's books
- Step 1: find the book name in the returned pages. Assumption if search result of author is the book, the book is author's
- Step 2: if there is no author page in wiki - return NaN (for further filtering)
- Step 3: if the page exists, parse author's bibliography page. My assumption, that there is less unnecessary data there
- Step 4: parse author's page

### Parsing rules

First the bibliography page is parsing.
I assume that all the rows after context section and the rows which are not headers are possible book's titles. So I try to find entities using scrapy and if the entity is 'Work_of_art', add it to the books' set
Also, in this case I parse the tables: make pandas dataframe and try to find column named like 'Title'. If the column is in the df, I save all the values to the books' set.
If there is no bibliography page algorithm tries to find "Bibliography section" and parses it using scrapy and entities search.

### Evaluation

Using sklearn library, it evalate precision, recall and F1 score. Algorithm uses label of only authors which were found in wiki.

If the book is not the book of the author it shows 100% accuracy (recall for False is 1.0)

Example of test_books.csv:
            
             precision    recall  f1-score   support

          0       0.71      1.00      0.83        10
          1       1.00      0.60      0.75        10

avg / total       0.86      0.80      0.79        20

### Explain few errors

+ George Orwell - 1984 - False: in wiki book name is "Nineteen Eighty-four" not number 1984
+ Anton Chekhov - Uncle Vanya - False: Vanya was recognized as entity name, need improvement in algorithm
+ Moliere - The Misanthrope - False: The sentance is partialy in French, so algorithm didn't find the entity "Le Misanthrope ou L'Atrabilaire amoureux (4 June 1666)—The Misanthrope, or, the Cantankerous Lover"

### Further improvements

+ to clean titles in db
+ to update scraping the page: get lines with books, clean it and save to the set
+ not to include to books such sections as journalist's works, lectures etc.
+ expand possible section to explore for books
+ use the trees and extract books from the plain text

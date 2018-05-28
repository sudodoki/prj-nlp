### End of sentence (EOS) recognition

+ Language: Python3
+ Required external libraries: pandas, sklearn, spacy
+ Script for preparing train data: students/oleg_m/07-sent-end/preprare_data.py
+ Script for preparing test data: students/oleg_m/07-sent-end/preprare_test_data.py
+ Prepared train file:
    + lucene_corpus.zip - scraped emails (~2500) from Lucene development team
    + test_data.zip - prepared data for testing
    + eos_ngrams.zip - prepared ngrams that were found as EOS
+ Python notebook with model evaluation: students/oleg_m/07-sent-end/sent_end.ipynb

### Data

Data is the emails from the Lucene dev team. The input data in json
Example:
```json
{'py/object': 'categorized_thread.CategorizedThread',
'category': 1.4,
'first_email': {'py/id': 1},
't_id': 2048,
'ending_date': '2004-08-23 23:05:38',
'authors': {'py/set': ['andraz.skoric@medius.si', 'otis_gospodnetic@yahoo.com']},
'starting_date': '2004-04-05 08:25:29',
'emails': [{'Delivered-To': 'apmail-jakarta-lucene-dev-archive@www.apache.org',
            'From': '"Andraz Skoric @ Medius" <andraz.skoric@medius.si>',
            'Return-Path': '<lucene-dev-return-5983-apmail-jakarta-lucene-dev-archive=jakarta.apache.org@jakarta.apache.org>',
            'Received': '(qmail 77202 invoked from network); 5 Apr 2004 08:26:28 -0000',
            'X-Accept-Language': 'en-us, en',
            'To': 'lucene-dev@jakarta.apache.org',
            'Message-ID': '<407117F9.7020400@medius.si>',
            'py/object': 'data.model.emails.memail.Email',
            'List-Post': '<mailto:lucene-dev@jakarta.apache.org>',
            'author': {
              'name': 'Andraz Skoric',
              'email': 'andraz.skoric@medius.si',
            },
            'Mailing-List': 'contact lucene-dev-help@jakarta.apache.org; run by ezmlm',
            'User-Agent': 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.5b) Gecko/20030903 Thunderbird/0.2',
            'Date': 'Mon, 05 Apr 2004 10:25:29 +0200',
            'Body': [['Hi,\n\nwhile using Lucene I implemented 2 new methods which i found useful \n(while implementing advanced search (find anything, find all)). If u would \nlike to use it here is a source file.\n\nLp, Andraz\n\n'],
            ]}]
}
```
The emails text are in emails.Body objects

### Features

I decided to prepare span feature for w-2, w-1, w, w+1 and w+2: isAlpha, isLower, isNumric, isPunctuation, POSTAG, lemma.
Also I collected 3-grams for each word and their lemmas: 'w-1 w w+1'. If that 3-gram is in the end of sentence I write it to the specific file.
Also the same for the 1-gramm was made.

Comments regarding the model preparation and results are in the Jupyter Notebook

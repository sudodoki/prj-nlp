from google_ngram_downloader import readline_google_store

fname, url, records = next(readline_google_store(ngram_len=2))
fname
'googlebooks-eng-all-5gram-20120701-0.gz'
url
'http://storage.googleapis.com/books/ngrams/books/googlebooks-eng-all-5gram-20120701-0.gz'
next(records)
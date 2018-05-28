#Run-on sentenses
* dowload training data from https://drive.google.com/open?id=13jXBNgZhLKg95JjyArKm-muoXSoP33TG
* replace @mensions with random name or he, she, it .. etc
* concat sentenses with splitter to one sentense with downcasing and splitter removal
* download bigrams from google,  downcase them, build dictionary
* prepare features based on prefious and next words, normal form, POS tag, count of ngrams dividet by ngram as a end of line
* try downsempling
* build  SGD and CRF clasifiers

# best results:
##SGD f1 for minor class 0.52 on validation set
##CRF f1 for minor class 0.56 on validation set
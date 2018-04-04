# Data

Data scraped from category [Інструменти та автотовари](https://rozetka.com.ua/ua/instrumenty-i-avtotovary/c4627858/)

Comments amount: 2192

Learn data: 1534 test data: 658 (70%/30%)

* greater than 3 - positive
* equal 3 - neutral
* less than 3 - negative

## First analyzer GaussianNB

Used CountVectorizer and GaussianNB from sklearn with default params

             precision    recall  f1-score   support

         -1       0.09      0.07      0.08        27
          0       0.07      0.05      0.06        56
          1       0.87      0.90      0.89       575
    avg/total     0.77      0.79      0.78       658



## Sentiment vocabulary

Downloaded all words from [tonal vocabulary](https://raw.githubusercontent.com/lang-uk/tone-dict-uk/master/tone-dict-uk-manual.tsv), which sentiment not equal 0

Converted data from CountVectorizer to array and increased count for words from tonal vocabulary

             precision    recall  f1-score   support

         -1       0.07      0.07      0.07        27
          0       0.08      0.75      0.15        56
          1       0.91      0.18      0.31       575
    avg /total    0.81      0.23      0.28       658

It increased precision a little bit but reduce other metrics.

## LinearSVC
Used LinearSVC from sklearn with default params

             precision    recall  f1-score   support

         -1       0.33      0.22      0.27        27
          0       0.35      0.11      0.16        56
          1       0.90      0.97      0.93       575
    avg/total     0.83      0.87      0.84       658

This classifier shows better results than other approaches.

Tweaking different params like tokenizing by characters or using n-grams didn't give any improvement.

I think the main problem is a low correlation between stars and text in comments also there are in comments there are some specific details about the instrument and no sentiment information.

Balancing learning data also didn't improve metrics.

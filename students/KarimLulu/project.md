# SMS Spam Detection

## Motivation
Проаналізувавши СМС дані свого телефону стало зрозуміло, що майже 90% СМС це різноманітний спам (таксі, доставки їжі/квітів/тощо, одяг, 
знижки і т.д.). І досі немає системи фільтрації для СМС, що швидше за все пов'язане з обмеженим доступом до даних користувачів.
Ця тема представляє інтерес з точки зору збору даних (див. наступну секцію), feature engineering (n-grams, entities, hand crafted 
indicators - contains emoticons/contains secial symbols/contains URLs/etc., length of the message, etc.), imbalacement handling 
(87% спаму в наявному датасеті та ~90% в моїх СМС). 

## Data
* http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
* також обдумую можливість зібрати дані зі свого телефону, друзів, та родичів (декілька вже погодилось)

## Resources
* http://cs229.stanford.edu/proj2013/ShiraniMehr-SMSSpamDetectionUsingMachineLearningApproach.pdf
* https://www.kaggle.com/c/sms-spam-detection
* https://github.com/abinayam/Spam-Detection-SMS
* https://medium.com/@kopilov.vlad/detect-sms-spam-in-kaggle-with-scikit-learn-5f6afa7a3ca2
* https://pdfs.semanticscholar.org/ee96/4e647c07fcc5793d937a561847bac807c1cd.pdf
* http://www.springer.com/cda/content/document/cda_downloaddocument/9789811057793-c2.pdf?SGWID=0-0-45-1612461-p181023245

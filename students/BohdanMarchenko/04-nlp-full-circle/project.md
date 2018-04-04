#### 1. які метрики для оцінки якості ви будете використовувати?
f1, recall, precission etc. 

#### 2. як ви бачите побудову бейзлайну?
> 1. An estimate for the sentiment. How "good" or "bad" is the sentiment of the company towards climate change. 
Range is from -2 to +2;

Naive Bayes based on article  https://gab41.lab41.org/a-tour-of-sentiment-analysis-techniques-getting-a-baseline-for-sunny-side-up-45730ec03330

> 2. Confidence level that this article is about given company, from 0 to 5

Тут може бути щось простіше, на зразок, наприклад, залежно від того, як часто назва компанії зустрічається в тексті.
> 3. Confidence level that this is about Climate Change, from 0 to 5

Те ж саме що в ппередньому пункті

#### 3. які готові рішення (включно зі state-of-the-art) вже існують та на яких підходах? (Якщо немає рішень для вашої задачі, дослідіть рішення для схожих задач.)

Є досить багато описаних вирішень подібних задач: 

http://ceur-ws.org/Vol-862/FEOSWp4.pdf
https://medium.com/@andriylazorenko/sentiment-analysis-news-python-part1-b6c5060c2437
http://www.aclweb.org/anthology/P07-1124
https://nlp.stanford.edu/courses/cs224n/2011/reports/nccohen-aatreya-jameszjj.pdf
https://www.researchgate.net/publication/301403733_Sentiment_Analysis_on_News_Articles_for_Stocks

Tools:
Pattern https://www.clips.uantwerpen.be/pages/pattern-dev#documentation
http://sentiwordnet.isti.cnr.it/

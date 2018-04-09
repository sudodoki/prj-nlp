#### 1. Perform initial data collection for your project.  
Моїм першим завданням є збір данних статей і новин, що потенційно можуть підійти для аналізу. 
Я пробую кілька готових рішень, наприклад 
https://newsapi.org/,  
http://dlab.berkeley.edu/blog/scraping-new-york-times-articles-python-tutorial, 
https://pypi.python.org/pypi/google_news_crawler/0.3.9
https://github.com/codelucas/newspaper
більшість з них, не підходить через нерелевантність результатів. 

Пробую знайти новини через кастомний гугл пошук https://support.google.com/customsearch/answer/70392?hl=en 
результати використання бібліотеки pattern знаходяться у папці scrape_result3  

#### 2. Devise and describe a way to collect data for your course project using crowdsourcing or from the users. Implement a proof-of-concept.
Другим етапом є аннотація даних для трьох параметрів сентимент аналізу:
1. An estimate for the sentiment. How "good" or "bad" is the sentiment of the company towards climate change. 
Range is from -2 to +2;
2. Confidence level that this article is about given company, from 0 to 5
3. Confidence level that this is about Climate Change, from 0 to 5
Для цього можна використати краусорсинг або mTurk. Приклад аннотації є в [01-sentiment-analysis-climate-change-articles.md](https://goo.gl/svvrNo)
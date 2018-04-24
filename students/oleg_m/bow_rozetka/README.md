## Task 5

+ Language: Python3
+ Required external libraries: numpy, pandas, sklearn, pymorphy2, langdetect
+ Script path for collecting data from rozetka: students/oleg_m/bow_rozetka/rozetka/rozetka/spiders/jobs.py
+ Scraped file:
    + rozetka/products_comments.zip - scraped comments (~6000) from [rozetka](http://rozetka.ua) product "Computers"
+ Run bash command for scrapping:
    + cd students/oleg_m/bow_rozetka/rozetka/
    + scrapy crawl jobs -o other-comments.csv
+ Python notebook with model evaluation: students/oleg_m/bow_rozetka/bow_modeling.ipynb

### Scraped data

Data was scraped from "Computers" section of rozetka.
Comments were selected if it is in Ukrainian (using external lib langdetect) and it has a rating by user. Also, this comment are divided into "common text", "advantages" and "weakness" of the product.
Example:
```json
{
    "Stars": 5,
    "Response": "<200 https://hard.rozetka.com.ua/ua/artline_business_b21_v01/p10099870/comments/>",
    "Normal_text": "Комп компактний і легкий. Встановив windows 7 ,поставив всі драйвери ,працює як часики.Для прогулянок по просторам інтернету,перегляду відео,легких іграшок більш ніж достатньо.Хто буде ставити windows 8.1 або 10 в комплектацію входять диски з драйверами для цих операційок,правда треба буде скопіювати їх на флешку . #мояраспаковка",
    "Pro": "компактний ,шустрий",
    "Con": "трошки шумний кулер блока живлення"
} 
```
This data was saved into csv file
+ Scrapy jobs script path: students/oleg_m/bow_rozetka/rozetka/rozetka/spiders/jobs.py

### Modeling

Please find more info according the modeling in Python notebook: students/oleg_m/bow_rozetka/bow_modeling.ipynb

### Further steps
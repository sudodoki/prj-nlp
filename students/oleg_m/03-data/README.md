### Task 1.1

The task was made in browser

### Task 1.2

+ Language: Python3
+ Required external libraries: BeautifulSoap
+ Required internal object: from utils import Stack
+ Script path: students/oleg_m/02-data/1-2_annts.py
+ External file: students/oleg_m/03-data/files/official-2014.zip
+ Run command: python3 students/oleg_m/02-sling/1-2_annts.py
<<<<<<< HEAD
+ Results: results/1-1_annts.txt
+ Result example:
> doc #1:  Counter({'other_miss': 20, 'correction': 15, 'type': 14, 'this_miss': 8, 'span': 7, 'same': 6})

> means that:
> + same annotations: 6
> + different correction: 15
> + different correction: 14
> + different span: 7
> + teacher_1 missed: 8
> + teacher_2 missed: 20
=======
>>>>>>> origin/master

### Task 2.1

#### 1. Test try for Belorussian language

This language was chosen because of the small file and clear language

+ Language: Python3
+ Script path: students/oleg_m/03-data/2-1_synonyms_extr_by.py
+ External file: students/oleg_m/03-data/files/bewiktionary-20180301-pages-articles-multistream.xml.zip
+ Run command: python3 students/oleg_m/03-data/2-1_synonyms_extr_by.py

#### 2. Synonyms for Portuguese language

File with Portuguese wikidictionary is cut because of size.
Full file can be downlaod from [wiktionary](https://dumps.wikimedia.org/ptwiktionary/20180320)

+ Language: Python3
+ Script path: students/oleg_m/03-data/2-2_synonyms_extr_pt.py
+ External file: students/oleg_m/03-data/files/ptwiktionary-20180301-pages-articles.xml.zip
+ Run command: python3 students/oleg_m/03-data/2-1_synonyms_extr_pt.py

+ Results: files/2-1_synonyms_extr_pt.txt
+ Results example:
> persa {1: {'persiano', 'pérsio', 'pérsico'}, 2: {'persiano', 'pérsio', 'pérsico'}}

### Task 2.2

Extracts posts of the default thread on the lviv forum and prints first n posts
I will implement this task with scrapy later

+ Language: Python3
+ Required external libraries: BeautifulSoap, scrapy
+ Script path: students/oleg_m/03-data/2-2_scraping_lviv.py
+ Results (of random thread of the forum): results/2-2_scraping_lviv.py
+ Results example:
> id: 71868
> link: http://forum.lvivport.com/threads/shukaju-spivavtora.71868/
> name: Шукаю співавтора
> pages: 7

> date: 2012-02-06 02:22:00
> author: Ulcer Scamp
> Здравствуйте, уважаемые участники форума! Вероятно, моё обращение...
>
> date: 2012-02-06 10:55:00
> author: Inga
> .
>
> date: 2012-02-06 11:39:00
> author: Милена
> Вопрос к тс, а Вам самому годочков сколько ? , ...
>

### Task 2.3

+ Language: SPARQL
+ Script path: students/oleg_m/03-data/2-3_SPARQL.sparql
+ Run script using [Virtuoso Editor](https://dbpedia.org/sparql)
+ Results: files/2-3_SPARQL.csv
+ Results example:
> person,name,related_person
>
> http://dbpedia.org/resource/Chesty_Puller,""Chesty""@en,http://dbpedia.org/resource/Lewis_Burwell_Puller,_Jr.
> 
> http://dbpedia.org/resource/Jeb_Bush,"#JE3OLUTION Bush"@en,http://dbpedia.org/resource/Bush_family

### Task 2.4

### NOT FINISHED
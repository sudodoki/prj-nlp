## Теми курсових проектів

### Проекти, для яких немає готових корпусів

Основна складність (і цікавість) цих проектів в тому, що для них паралельно доведеться будувати і датасет, і модель. Більшість NLP-задач реального життя саме такі :) 

1. Визначення зв’язків між сутностями на основі даних з Wikipedia, Freebase, DBPedia тощо для української мови.
2. Визначення суб’єктивних висловлювань в текстах новин (зокрема, новин українською мовою).
3. Генерація поезії. Доступні дані: сайти з віршами, словники рим тощо.
4. POS-tagging для української мови. Проанотованих корпусів немає, але є граматичний словник та корпуси сирих текстів.
5. Перевірка правопису для української мови. Дані можна проанотувати через LanguageTool; також схожий проект є [тут](https://github.com/khrystyna-skopyk/ukr_spell_check).
6. Автоматична генерація відповідей на запитання на сайті https://ukrainian.stackexchange.com (та інших SE сайтах).
7. Реалізації алгоритму побудови векторів [NNSE](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4497373/) - це задача для тих, хто хоче заглибитись в математичну частину лінгвістичних алгоритмів.
8. Пошук плагіату в україномовних текстах. Дані можна видобувати на: сайтах з рефератами, сайтах новин (передруківки).
9. Автореферування текстів конкретної тематики.

### Проекти, для яких є готові корпуси

Передбачається, що якщо ви обрали один з цих проектів, то ви будете шукати і використовувати додаткові дані, а не просто відтворювати вже готові результати.

10. Визначення тролінгу, образливих коментарів чи критики. Можна використати дані:
    - https://www.kaggle.com/c/jigsaw-toxic-comment-classification-challenge
    - https://www.kaggle.com/c/detecting-insults-in-social-commentary
11. Визначення подібності тексту чи перефразування тексту. Можна на основі новин зробити (різні ресурси часто перепощують ту саму інформацію іншими словами), а також використати дані:
    - https://www.kaggle.com/c/quora-question-pairs/data
    - http://alt.qcri.org/semeval2016/task1/
12. Визначення авторства чи визначення статі/віку/соціальної групи автора. Можна будь-яких авторів будь-якою мовою назбирати. Приклад даних: https://www.kaggle.com/c/spooky-author-identification.
13. Визначення емоцій/сентиментів для конкретного домену. Можна своїх даних наскрейпити зі споживацьких сайтів. Також є багато готових корпусів, наприклад, про емоції є тут: https://competitions.codalab.org/competitions/17751.
14. Визначення найбільш ймовірного закінчення історії. Можна нагенерувати своїх даних з невеличких текстів, а також є https://competitions.codalab.org/competitions/15333.
15. Визначення значень слів. Є, наприклад, https://nlpub.github.io/russe-wsi-kit/.
16. Моделювання граматичних помилок, які роблять ті, хто вивчає мову:
    - http://sharedtask.duolingo.com/ (активна задача від DuoLingo).
17. Виправлення граматичних помилок, які роблять ті, хто вивчає мову:
    - http://cl.naist.jp/nldata/lang-8/
    - http://www.comp.nus.edu.sg/~nlp/conll14st.html
18. Визначення іронії, сарказму чи мовних каламбурів:
    - https://competitions.codalab.org/competitions/17468#learn_the_details
    - http://alt.qcri.org/semeval2017/task7/
19. Побудова паралельного корпусу:
    - https://comparable.limsi.fr/bucc2017/bucc2017-task.html.
20. Machine Comprehension using Commonsense Knowledge:
    - https://competitions.codalab.org/competitions/17184.

### Задачі від компаній

1. Grammarly: визначення стилю речення (formal/informal). Дані: частина корпусу [Yahoo answers](https://webscope.sandbox.yahoo.com/catalog.php?datatype=l) проанотованого за ознакою стилю написання.

### Додаткові джерела даних та ідей

1. Гарна збірка NLP-задач зі статтями і даними:
    - https://github.com/Kyubyong/nlp_tasks
2. Діалоги з мультика "South Park":
    - https://www.kaggle.com/tovarischsukhov/southparklines
3. Корпуси пісень:
    - https://www.kaggle.com/mousehead/songlyrics
    - https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics
4. Відгуки про сорти вин:
    - https://www.kaggle.com/zynicide/wine-reviews
5. Споживацькі відгуки про їжу:
    - https://www.kaggle.com/snap/amazon-fine-food-reviews
6. SMS-спам:
    - http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
7. Додаткові джерела даних:
    - https://www.kaggle.com/datasets?sortBy=relevance&group=featured&page=1&pageSize=20&size=all&filetype=all&license=all&tagids=11208
    - http://data.gov.ua/datasets
    - http://noisy-text.github.io
    - CONLL shared tasks: http://www.conll.org/previous-tasks
    - SemEval shared tasks: http://alt.qcri.org/semeval2018/index.php?id=tasks

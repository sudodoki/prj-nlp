# lviv-forum-scraper

```
sudo apt install libcurl4-openssl-dev
stack build
stack exec lviv-forum-scraper -- data/cache
```

Example output upon first run can be seen (there's a lot since I print
out textual version of downloaded content):
https://gist.github.com/k-bx/eaae8ec341503d632869263692297995

```
$ stack build && stack exec lviv-forum-scraper ./data/cache/
WARNING: Ignoring out of range dependency (allow-newer enabled): template-haskell-2.12.0.0. regex requires: >=2.7 && <2.12
Queue: 
[DownloadTopicsListPage "forums/filosofija-ta-moral.88/"]
Downloading: forums/filosofija-ta-moral.88/
Got file from cache: forums/filosofija-ta-moral.88/
-----
Received a result:
18 лют 2018 - Як батько оцінює «діяльність» свого сина? - threads/jak-batko-ocinjuje-dijalnist-svogo-sina.114780/
2 бер 2018 - Григорій Сковорода: пророк чи пихатий порок? - threads/grigorij-skovoroda-prorok-chi-pixatij-porok.114824/
22 лис 2011 - Сурогатне материнство - threads/surogatne-materinstvo.71185/
22 бер 2013 - Притчі про життя ... - threads/pritchi-pro-zhittja.80674/
8 лип 2016 - ‪#‎яНеБоюсьСказати‬ - threads/janebojusskazati.111963/
30 січ 2015 - В чому щастя людини? - threads/v-chomu-schastja-ljudini.107384/
12 гру 2017 - Хто винуватий в загибелі Шевченкової Катерини? - threads/xto-vinuvatij-v-zagibeli-shevchenkovoji-katerini.114479/
5 лис 2017 - Діалог Slav&Коммунарец. Істина - threads/dialog-slav-kommunarec-istina.114266/
18 лис 2017 - Творення світу речовинного з точки зору езотеричного креаціонізму. - threads/tvorennja-svitu-rechovinnogo-z-tochki-zoru-ezoterichnogo-kreacionizmu.114340/
1 кві 2016 - Соціальні мережі - зло, або залайкай мене до інфаркту. - threads/socialni-merezhi-zlo-abo-zalajkaj-mene-do-infarktu.111393/
19 бер 2009 - Магия! вымысел или нет? - threads/magija-vymysel-ili-net.61846/
9 кві 2009 - Чи вірите ви в прикмети? - threads/chi-virite-vi-v-prikmeti.62232/
13 кві 2017 - Сутність  Бога. - threads/sutnist-boga.113360/
22 кві 2017 - Президентам, Парламентам, Керівникам! - threads/prezidentam-parlamentam-kerivnikam.113388/
13 кві 2017 - Сутність та призначення людини. - threads/sutnist-ta-priznachennja-ljudini.113361/
1 бер 2017 - Просвіта обов"язок кожного з нас ! - threads/prosvita-obov-jazok-kozhnogo-z-nas.113154/
14 гру 2007 - Доля чи вибір? - threads/dolja-chi-vibir.31034/
11 лют 2013 - Про допомогу - threads/pro-dopomogu.79817/
26 кві 2011 - Люди добрі чи злі? - threads/ljudi-dobri-chi-zli.69024/
10 жов 2008 - "Я" та екологія - threads/ja-ta-ekologija.42138/
["forums/filosofija-ta-moral.88/",
 "forums/filosofija-ta-moral.88/page-2",
 "forums/filosofija-ta-moral.88/page-3",
 "forums/filosofija-ta-moral.88/page-4",
 "forums/filosofija-ta-moral.88/page-5",
 "forums/filosofija-ta-moral.88/page-6",
 "forums/filosofija-ta-moral.88/page-22"]
```

Results will be stored in `data/cache/`.

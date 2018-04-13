# Які метрики для оцінки якості ви будете використовувати?
Accuracy, precision, recall, F-measure окремо для оцінки якості визначення класу документа і абзацу. Є сенс використати міру (яку?) для крос-перевірки результатів на БрУК і корпусі публіцистики, що має бути зібраним для задачі.
# Як ви бачите побудову бейзлайну?
Як features (характеристик?) для бейзлайну беремо n-грами довжиною 1-3 для слів та POS тегів. Для класифікації можна перевірити Multinomial & Bernoulli Naive Bayes, для абзаців K-means. Треба показати список обраних характеристик та оцінити їх внесок, попередньо з допомогою I(C;Wt) (average mutual information with the class variable and the absence
or presence of a word in the document [McCallum 1998])
# Які готові рішення (включно зі state-of-the-art) вже існують та на яких підходах? (Якщо немає рішень для вашої задачі, дослідіть рішення для схожих задач.)
Про state-of-the-art говорити не доводиться, я не знайшла загальноприйнятого корпусу для стилістичного аналізу. Найбільш поширеними рішеннями є класифікація на POS- та лексичних N-грамах, найбільш поширених словах. Є підхід на синтаксичних N-грамах (dependency). З нових підходів можна виділити вектори речень для нейронних мереж.
## Програмні пакети
+ Stylo for R https://sites.google.com/site/computationalstylistics/home пакет для стилометрії текстів
## Статті
+ Gianfortoni, Philip, David Adamson, and Carolyn P. Rosé. "Modeling of stylistic variation in social media with stretchy patterns." Proceedings of the First Workshop on Algorithms and Resources for Modelling of Dialects and Language Varieties. Association for Computational Linguistics, 2011. - виділення характеристик окремо від теми, опис можливих варіантів бейзлайну, метрики оцінки якості при варіаціях розміру даних для навчання, gender variations for social media 
+ Khosmood, Foaad, and Robert A. Levinson. "Automatic natural language style classification and transformation." BCS Corpus Profiling Workshop, London, UK. sn, 2008. - обговорення стилістичних маркерів, перетворення тексту згідно зі стилем (rule & lexicon based), метрики для генерації стилю подібно до вибраних літературних текстів
+ Rehbein, Ines, and Felix Bildhauer. "Data point selection for genre-aware parsing." Proceedings of the 16th International Workshop on Treebanks and Linguistic Theories. 2017. - розрізнення особливостей, залежних від домену, і залежних від жанру, dependencies, LDA for topic distribution
+ Stamatatos, Efstathios, Nikos Fakotakis, and George Kokkinakis. "Automatic text categorization in terms of genre and author." Computational linguistics 26.4 (2000): 471-495.- style markers is divided into three levels—token level, syntax level,
and analysis level, logistic regression
+ Kabbara, Jad, and Jackie Chi Kit Cheung. "Stylistic transfer in natural language generation systems using recurrent neural networks." Proceedings of the Workshop on Uphill Battles in Language Processing: Scaling Early Achievements to Robust Methods. 2016. - перетворення документа зі збереженням значення і зміною стилю, LSTM-based RNN model
+ McCallum, Andrew, and Kamal Nigam. "A comparison of event models for naive bayes text classification." AAAI-98 workshop on learning for text categorization. Vol. 752. No. 1. 1998. - 
про класифікацію NB та виділення характеристик
+ Lin, Grace, and Marilyn Walker. "Stylistic Variation in Television Dialogue for Natural Language Generation." Proceedings of the Workshop on Stylistic Variation. 2017. - feature selection and character-level dialog generation for the Big Bang Theory
+ Wang, L. Z. "News authorship identification with deep learning." (2017). - s, sentence-level Recurrent
Neural Network (RNN)
+ Santini, Marina. "State-of-the-art on automatic genre identification." (2004). - огляд, висновок про нечіткість термінології
+ Oraby, Shereen, Sheideh Homayon, and Marilyn Walker. "Harvesting Creative Templates for Generating Stylistically Varied Restaurant Reviews." arXiv preprint arXiv:1709.05308 (2017). - виділення типових патернів для використання з генерацією рев'ю 
+ Mandravickaite, Justina, and Tomas Krilavičius. "Stylometric Analysis of Parliamentary Speeches: Gender Dimension." Proceedings of the 6th Workshop on Balto-Slavic Natural Language Processing. 2017. - Stylo, gender stylometric, Canberra distance, за найбільш частотними словами
+ Cho, Kyunghyun. "Strawman: an Ensemble of Deep Bag-of-Ngrams for Sentiment Analysis." arXiv preprint arXiv:1707.08939 (2017). - deep bag-of-ngams для подальшого розгляду
+ Santini, Marina, et al. "Automatic genre identification: Issues and prospects." Journal for Language Technology and Computational Linguistics, JLCL ISSN (2009): 0175-1336.
+ Kiros, Ryan, et al. "Skip-thought vectors." Advances in neural information processing systems. 2015. - sentence representation
+ Dai, Andrew M., and Quoc V. Le. "Semi-supervised sequence learning." Advances in Neural Information Processing Systems. 2015.
+ Tambouratzis, George, et al. "Automatic Style Categorisation of Corpora in the Greek Language." LREC. 2000. - for morphology rich language
+ Michos, Stephanos E., Nikos Fakotakis, and Georgios Kokkinakis. "Enhancing text retrieval by using advanced stylistic techniques." Journal of Intelligent and Robotic Systems 26.2 (1999): 137-156.
+ Sboev, Alexander, et al. "Automatic gender identification of author of Russian text by machine learning and neural net algorithms in case of gender deception." Procedia Computer Science 123 (2018): 417-423. - розглянуто багато алгоритмів в конексті визначення статі 

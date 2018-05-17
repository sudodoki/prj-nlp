В якості тренувальних даних було взято по 1000 речень кожного типу (entailment, contradiction, neutral)
В якості тестових даних - 988 речень.

В якості features було використано similiraty з WordNet http://www.nltk.org/_modules/nltk/corpus/reader/wordnet.html

- path_similarity
- lch_similarity
- wup_similarity
- res_similarity
- jcn_similarity
- lin_similarity

Logistic Regression Classifier
               precision    recall  f1-score   support

contradiction      0.489     0.615     0.545       325
   entailment      0.520     0.687     0.592       339
      neutral      0.397     0.160     0.229       324

  avg / total      0.469     0.491     0.457       988
  
  
Для покращеня моделі було додано кількість слів в гіпотезі, які мають повне співпадіння хоча б одного значення із SynSets для слова з тексту.
Також було змінено параметри логістичної регресії: 
lrc = LogisticRegression(random_state=random_state, multi_class='multinomial', solver='saga', C=0.05, n_jobs=-1, max_iter=1000)

Logistic Regression Classifier
               precision    recall  f1-score   support

contradiction      0.510     0.545     0.527       325
   entailment      0.557     0.617     0.585       339
      neutral      0.470     0.386     0.424       324

  avg / total      0.513     0.517     0.513       988
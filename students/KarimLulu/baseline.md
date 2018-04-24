## Код
Код з бейзлайном - https://github.com/KarimLulu/sms-spam-detection/blob/master/notebooks/0.1-klulu-baseline.ipynb

## Методологія
Було застосовано свою реалізацію NB, MultinomialNB та XGBoost. 
Ознаки були побудовані за допомогою TF-IDF на 3500 символьних 3-граммах.

## Результати
Найкращі результати було отримано на MultinomialNB:

AUC: 0.965  
Recall: 0.908  
Precision: 0.969  
F1: 0.937  
accuracy: 0.972  

      	pred_ham  pred_spam
	ham        895          8
	spam        25        247

## Висновки
Простий бейзлайн без особливого feature engineering показав дуже високі результати, але система є
мовнозалежною. Наступні кроки повинні включати розробку мовнонезалежних підходів та ознак. 
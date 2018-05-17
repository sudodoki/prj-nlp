# які метрики для оцінки якості ви будете використовувати?
ROUGE (1, 2, L), BLEU
можливо ще спробую додати свої метрики в ході імплементації
# як ви бачите побудову бейзлайну?
На мою думку за бейзлайн можна взяти список слів витягнутих з статті (очищеної від stopwords) за допомогою tf-idf
# які готові рішення (включно зі state-of-the-art) вже існують та на яких підходах? (Якщо немає рішень для вашої задачі, дослідіть рішення для схожих задач.)
 http://atour.iro.umontreal.ca/rali/sites/default/files/publis/TAC10-RALI.pdf
 https://arxiv.org/pdf/1801.10198.pdf
 https://einstein.ai/research/your-tldr-by-an-ai-a-deep-reinforced-model-for-abstractive-summarization
Датасети:
https://cs.nyu.edu/~kcho/DMQA/ (CNN/Daily Mail dataset)
Wikipedia
    • Зібрати датасет схожий на  CNN/Daily Mail з українських сайтів (можна спробувати новини на різні теми), наукових статтей

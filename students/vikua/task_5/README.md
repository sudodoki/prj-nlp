### Solution consists of following Jupyter notebooks and python scripts:

- `rozetka_scrapper.py` - scrapy script to load reviews data. I've loaded
around ~60k reviews with huge data skew towards positive reviews (looks like
people tend to write good reviews:))
Raw data is available [here](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_5/reviews.json)
- `initial_model.ipynb` - first line attack - BOW and TF-IDF algorithms over raw data (non processed)
and Random Forest classifier. Classifier gives poor performance - .98 F1 score on train and .57 on test
(stratified CV partitioning)
When dataset is downsampled - F1 score is around 0.35
Interestingly, TF-IDF performance is worse then BOW
- `exploration.ipynb` - notebook with data exploration - languages analysis, smiles (happy, sad), etc.
- `improved_model.ipynb` - in this notebook I made data cleaning - deleted some punctuation (except ?, !, ), (),
normalized and lemmatized data, deleted stopwords.
Single RandomForest classifier performance with balanced class_weight (penalized majority classes) gave 0.58 on validation.
Stacked model with RandomForest gini, RandomForest entropy, ExtraTrees, AdaBoost, GradientBoosted trees, SGD (elastic net)
as first level and LightGBM as second level gave 0.63 F1 score.
I've analyzed confusion matrix and found that 4 and 5 are very confused very often as well as 1 and 2 classes.
So as next attempt I combined 1 and 2 classes and 4 and 5 and trained same models on three classes. Results:
Single RF - 0.78, stacked model - 0.8

Word2vec trained on full data didn't help to improve F1 score, although gave good results in terms of words
similarity. One caveat - it didn't learn to match same words in different languages, I think it might be an issue.

As an improvement to this results it worth trying to use pertained w2v (but language issue persists) or use char2vec or something.

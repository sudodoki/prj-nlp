## Description
My task of converting natural language search queries into the structured form has 3 steps (subtasks):

1. Split search query into the phrases
```
   Find all Marjories that replied to negative post and downloaded exhibitor file'
   ->
   Marjories, replied to negative post, downloaded exhibitor file
```
2. Classify each phrase to one of category:
   `post, like, reply, view, profile info, ...`

3. Do rule-based argument extraction for each of the phrase based on it's class

For this task I focus on second task, assuming I have successfully splitted data. 

## Data preparation
* Mannualy split data into phrases
* Replace names, companies, search terms, etc with placeholders
* Retain only unique phrases
* Manually add class to each example
* Split data into train/test sets

## Training
I've tried to train Naive Bayes classifier and got pretty good results in accuracy: `0.954`. But after upsampling data I've got even better result: train accuracy = `0.972` and test accuracy = `0.971`. 
Here is breakdown on f1/prec/rec by classes:

|ad|event|file|like|note|poll|post|profile|rate|reply|session|view
------------------------------------------------------------------
prec|0.8809523809523809|1.0|1.0|0.9767441860465116|0.9512195121951219|0.972972972972973|0.9629629629629629|1.0|1.0|0.925|0.9722222222222222|1.0
rec|1.0|1.0|0.9047619047619048|1.0|1.0|1.0|0.8125|0.9090909090909091|1.0|1.0|1.0|1.0
f1|0.9367088607594937|1.0|0.9500000000000001|0.988235294117647|0.975|0.9863013698630138|0.8813559322033898|0.9523809523809523|1.0|0.961038961038961|0.9859154929577464|1.0

Well, we see that results are pretty good, although precision of ad and recall of post might be imrpoved.

Next steps:
* Write classificator for splitting: rule-based on ml-based
* Do argument extraction

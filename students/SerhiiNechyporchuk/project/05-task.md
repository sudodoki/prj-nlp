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

| | ad |event|file|like|note|poll|post|profile|rate|reply|session|view |
|-|-----|-----|----|----|----|----|----|-------|----|-----|-------|-----|
|prec|0.88|1.0|1.0|0.97|0.95|0.97|0.96|1.0|1.0|0.92|0.97|1.0|
rec|1.0|1.0|0.90|1.0|1.0|1.0|0.81|0.91|1.0|1.0|1.0|1.0
f1|0.93|1.0|0.95|0.98|0.98|0.98|0.88|0.95|1.0|0.96|0.98|1.0

Well, we see that results are pretty good, although precision of ad and recall of post might be imrpoved.

Next steps:
* Write classificator for splitting: rule-based on ml-based
* Do argument extraction

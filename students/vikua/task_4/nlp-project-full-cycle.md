## Task 4

Prerequisites are same as previous

There are a couple of helper scripts, which are not related to rule based model itself,
but those helped to gather and label data. 

#### pyspark_data_load.py
IMBD movies dataset was used to get TOP N movies (in order to work with most popular movies :))
This script loads `title.basics.tsv` and `title.ratings.tsv` files, joins them, calculates rating based 
on IMDB 1-10 scale rating and number of users watched the movies, and saves CSV file with top N films. 
Example can be found [here](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_4/top_n.csv)

Although IMBD also has file with actors for each movie, that data is not full - file contains only several 
movies particular actor is known for, not all of them. I wanted to use it as reference DB, but this issue 
changed my mind. (For example Emma Stone is not listed as actress in Zombieland movies, but wikipedia
has this information. This could lead to false positives due to data issues)

#### sparql_loader.py
So in order to create reference DB, I decided to use DBPedia. This scipt loads data from 
DBPedia and has two modes. 
- Load  DBPedia url for each movies (`movies` mode). This query uses data from previous step (top N movies)
and loads their DBPedia urls. It may load more them one url for single movie (because it is `contains` query) 
so result JSON file requires manual corrections. I've loaded DBPedia urls for 300 movies and manually corrected them 
to have just one url per movie. Result is [here](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_4/top.json).
- Load casts for each movie (`cast` mode). This query uses urls from previous step and loads all Person entities 
which starred in particular film. [Here is the result](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_4/movies_casts.json)

#### load_wiki_pages.py
Once reference DB is ready, it is time to load some data from Wikipedia. I use names of DBPedia entities for 
movies are used as url paths for HTML page on Wiki. This script iterates over movies loaded into reference 
json DB, scraps html and saves as html files. 
[(Example zip file with html pages)](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_4/wiki_html.zip)

#### label_data.py 
This script loads html pages saved on previous step and parses them to extract text. Note that 
it skips Cast section. 
Also it labels data automatically - for each movie it loads cast from reference DB and attaches it to 
text as new `labels` field in data frame. 
Result training sample is labels : text pairs.
[(Example zip file)](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_4/labelled_data.zip)

#### rules.py
Contains all rules in model. Right now there are three rules, which are based on dependencies.

#### rule_model.py
Actual model, which accepts text and generates predictions. 

I use F1 score metric. 
Considering the fact that each data sample (text about single movie) may have different number of labels (actors)
and those labels may not intersect between different movies, I compute F1 measure for each movie separately.

So for example there are five actors in specific movie, but model predicted 4 and only 3 are correct. 
I binarize two lists of actors to make two one-hot vectors of the same shape and compute F1 on those. 
So F1 score is computed for each movie separately and them MEAN is taken. 

Example with initial results: 

```
$ python3 rule_model.py -i out  
                                                                                      ⏎ vikua/task-4 ✚ ✱ ◼
Train F1 score = 0.40944665104764544
Test F1 score = 0.3774518937262914
```
The values are quite low, mainly because there are only 3 rules and also examples where only last name or first name 
is detected are considered as negative. 

Due to specifics of domain, dynamic nature of labels, I didn't build confusion matrix (and I'm not sure it makes sence here as 
it is more of milti label classification). 

[Result CSVs](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_4/result.zip)

Further analysis is in model.ipynb 
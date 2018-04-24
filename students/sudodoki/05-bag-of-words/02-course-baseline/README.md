# Course Baseline

Once again, task it to predict labels of (hopefully, one day, dynamic) relationships between characters in prose text / fiction.

## Annotations

For this baseline I used annotations that can be found in [dbamman/characterRelations](https://github.com/dbamman/characterRelations).

## /books

Long story short - got books based on list of titles + authors in relationships dataset and downloading from https://github.com/GITenberg + manually downloading one that is missing from GIT dump.

## bookNLP_output

Used great [book-nlp](https://github.com/dbamman/book-nlp) project to provide bunch of useful information about books. In baseline information about characters, paragraphs and lemmas of each token was used.

## What was done

0. Remove annotations with 'NR' affinity
1. Given there are multiple annotators annotating same books, used mapping (neg -> 0, neutral -> 0.5, pos -> 1) for affinity and averaged it for each pair.
2. Based on book_nlp output for characters, selected those pairs that have at least 5 paragraphs where both were mentioned and built a table [id1, id2]: [paragraphIds]
3. For each pair do a sentimental analysis for all paragraphs' concatenated tokens (based on averaging sentiWordnet sentyment)
4. Tried mapping character ids to names used in annotations, with only few exceptions, seems to work ok~ish
5. Our predictor is just big dataframe with list of (book_name char_1 char_2 affinity), so just using lookup in it to provide answer or 0.5 if it's not present.
6. Based on predicted values and given they are continuous, using mean_squared_error to measure performance gives us error of `0.19`. Translating continuous values to labels (by dividing [0, 1] into 3 parts for neg, neu, pos), we can also get classification report
```
              precision    recall  f1-score   support

   negative       0.34      0.11      0.17       397
    neutral       0.22      0.87      0.36       313
   positive       0.61      0.09      0.16       759

avg / total       0.46      0.26      0.20      1469
```

## Issues

1. Annotated data having issues:
  a) different annotators possibly having different order in pairs
  b) NR still present with some comments
2. Book sources having issues:
  a) different editions of same book might be available with different level of changes (starting with additions / deletions ending with non-rendering font using Æ and others)
  b) licensing need to be removed as it introduces extra noise
3. book-nlp output having issues:
  a) as is, it causes issues when loaded via pandas as there're ocasionally a label for token that is too long or some other issues
  b) might result in same character being represented as multiple
4. current algorithm having issues:
  a) no way to limit characters to only 'important once', so we can do some sort of validation but either external part should be responsible for selecting the main characters (and it's not yet fact those would be present in annotated relations) or current system updated to account for that
  b) sentiment of sentences mentioning two characters isn't the best way, some papers mention other ways of doing this

## Next steps

1. There are still long way to go with this amount of data and this baseline: tuning sentiment, removing stopwords, fixing all of the issues.
2. I have another dataset that need some transforming to be compatible with current one
3. Trying other baselines.
# rozetka

Had a pretty bad experience with this one and definitely not proud of
the result, but happy with some of the knowledge I got.

First, I was irritated by the fact that I didn't understand the
simplest thing which is a Naive Bayes classifier, so instead of using
a ready-made API from scikit-learn, I decided to actually get the
knowledge and read about it. Reading from Google and Scientific papers
is definitely not the best experience, I would prefer a well-made book
on the topic (I've ordered some).

As a result, I've implemented the Multinomial Naive Bayes as per
"Multinomial Naive Bayes for Text Categorization Revisited" paper. See
https://github.com/k-bx/nlp/blob/master/naiveml/src/NaiveML/MultinomialNaiveBayes.hs
for the implementation. I think I have a much better understanding of
the algorithm now, although still can't say I understand it well
enough to be able to easily tune it further as was done in the paper.

Now, to the task itself. There were problems with my chosen categories
not having enough ukrainian comments and then other problems which led
to spending too much time switching to another category, so I gave up
basically. I've got 131 comments out of men's sneakers, Naive Bayes
did 0.80 accuracy. I've tried helping it by adding every word in tone
dict as a separate test set document with its tone set, but it only
made things worse. I don't have a good answer why as of this moment.

```
$ stack build && stack exec rozetka
rozetka-0.1.0.0: build (exe)
2018-04-08 22:22:34.544329: [debug] Number of ukrainian comments with score: 131
@(src/Main.hs:245:3)
2018-04-08 22:22:36.393025: [debug] Accuracy: 0.8077
@(src/Main.hs:256:3)
2018-04-08 22:22:36.393102: [debug] Adding the tones now
@(src/Main.hs:257:3)
2018-04-08 22:22:36.393167: [debug] Reading tone-dict-uk-auto.tsv
@(src/Main.hs:220:3)
2018-04-08 22:22:36.394853: [debug] Reading tone-dict-uk-manual.tsv
@(src/Main.hs:223:3)
2018-04-08 22:23:29.359122: [debug] Accuracy: 0.1154
@(src/Main.hs:264:3)
```

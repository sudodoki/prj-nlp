# factcheck

Here's an example result:

```
$ stack build
$ stack exec factcheck -- /home/kb/workspace/standford-corenlp/stanford-corenlp-full-2018-02-27/ /tmp/factcheck-cache
Got documents out of cache: 338
[BookParenthsYear]
Correct claim! Book: Book {title = "The End of Faith: Religion, Terror, and the Future of Reason", mpubYear = Just 2004}
Sentence: 
Harris 's first book _ The End of Faith _ ( 2004 ) won the PEN/Martha Albrand Award for First Nonfiction .

[BookParenthsYear]
Correct claim! Book: Book {title = "The Moral Landscape: How Science Can Determine Human Values", mpubYear = Just 2010}
Sentence: 
In _ The Moral Landscape _ ( 2010 ) , he argues that science answers moral problems and can aid human well-being .

[BookParenthsYear]
Correct claim! Book: Book {title = "Free Will", mpubYear = Just 2012}
Sentence: 
[ 2 ] He published a long-form essay _ Lying _ in 2011 , the short book _ Free Will _ in 2012 , _ Waking Up : A Guide to Spirituality Without Religion _ in 2014 and , with British writer Maajid Nawaz , _ Islam and the Future of Tolerance : A Dialogue _ in 2015 .

[BookParenthsYear]
Correct claim! Book: Book {title = "Lying", mpubYear = Just 2011}
Sentence: 
[ 2 ] He published a long-form essay _ Lying _ in 2011 , the short book _ Free Will _ in 2012 , _ Waking Up : A Guide to Spirituality Without Religion _ in 2014 and , with British writer Maajid Nawaz , _ Islam and the Future of Tolerance : A Dialogue _ in 2015 .

[HePublishedIn]
Correct claim! Book: Book {title = "Lying", mpubYear = Just 2011}
Sentence: 
[ 2 ] He published a long-form essay _ Lying _ in 2011 , the short book _ Free Will _ in 2012 , _ Waking Up : A Guide to Spirituality Without Religion _ in 2014 and , with British writer Maajid Nawaz , _ Islam and the Future of Tolerance : A Dialogue _ in 2015 .

[BookParenthsYear]
Correct claim! Book: Book {title = "Letter to a Christian Nation", mpubYear = Just 2006}
Sentence: 
- _ Letter to a Christian Nation _ ( 2006 ) .

[BookParenthsYear]
Correct claim! Book: Book {title = "Lying", mpubYear = Just 2011}
Sentence: 
- _ Lying _ ( 2011 )

[BookParenthsYear]
Correct claim! Book: Book {title = "Free Will", mpubYear = Just 2012}
Sentence: 
- _ Free Will _ ( 2012 ) .

[BookParenthsYear]
Correct claim! Book: Book {title = "Islam and the Future of Tolerance: A Dialogue", mpubYear = Just 2015}
Sentence: 
- _ Islam and the Future of Tolerance _ ( 2015 )
```

If you don't want to launch the CoreNLP, please download the attached
cache and ungzip it somewhere like /tmp/factcheck-cache.

Pieces in text which I've found pointing to the books and their publish year, ones which my rules covered are marked with a checkbox:

  - [ ] Harris's first book The End of Faith (2004)
  - [x] In The Moral Landscape (2010)
  - [x] published a long-form essay Lying in 2011
  - [ ] , the short book Free Will in 2012
  - [ ] , Waking Up: A Guide to Spirituality Without Religion in 2014
  - [ ] with British writer Maajid Nawaz, Islam and the Future of Tolerance: A Dialogue in 2015
  - [ ] In Waking Up: A Guide to Spirituality Without Religion (2014)
  - [x] The End of Faith: Religion, Terror, and the Future of Reason (2004). ISBN 0-393-03515-8
  - [x] Letter to a Christian Nation (2006). ISBN 0-307-26577-3
  - [ ] The Moral Landscape: How Science Can Determine Human Values (2010). ISBN 978-1-4391-7121-9
  - [x] Lying (2011) ISBN 978-1940051000
  - [x] Free Will (2012). ISBN 978-1451683400
  - [ ] Waking Up: A Guide to Spirituality Without Religion (2014) ISBN 978-1451636017
  - [x] Islam and the Future of Tolerance (2015) ISBN 978-0674088702

So, the precision is 100%, recall is 7/14=0.5

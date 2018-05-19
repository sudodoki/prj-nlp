# Сторінки з Wikipedia про фільми на основі "Попелюшки" Шарля Перро
## SPARQL-запити

Для бази даних беремо дві категорії з вікі, які містять слово "Cinderella", і загалом дублюють одна одну. 
Сторінки з цих категорій використаємо як positive testset

SELECT DISTINCT
?movie ?movieLabel ?wiki ?date ?yearcat
WHERE {
?movie a dbo:Film.
?movie dct:subject ?cat.
OPTIONAL { ?movie dbo:releaseDate ?date }

?movie rdfs:label ?movieLabel.
FILTER (lang(?movieLabel) = 'en')

?cat rdfs:label ?catLabel.
FILTER (lang(?catLabel) = 'en')
FILTER regex(str(?catLabel), "Cinderella", "i").

OPTIONAL 
{
?movie dct:subject ?yearcat.
FILTER regex(str(?yearcat), "\\d{4}_films", "i").
}

?movie foaf:isPrimaryTopicOf ?wiki

}

Для negative testset беремо дві окремі категорії - книги на основі Попелюшки та фільми про хіппі

SELECT 
?movie ?movieLabel ?wiki
WHERE {
?movie a dbo:Film.
?movie dct:subject dbc:Hippie_films.

?movie rdfs:label ?movieLabel.
FILTER (lang(?movieLabel) = 'en').

?movie foaf:isPrimaryTopicOf ?wiki

} 


SELECT DISTINCT
?book ?bookLabel ?wiki 
WHERE {
?book a dbo:Book.
?book dct:subject ?cat.

?book rdfs:label ?bookLabel.
FILTER (lang(?bookLabel) = 'en').

?cat rdfs:label ?catLabel.
FILTER (lang(?catLabel) = 'en')
FILTER regex(str(?catLabel), "Cinderella", "i").

?book foaf:isPrimaryTopicOf ?wiki

}

## Формуємо csv-файл та завантажуємо сторінки у текстовому вигляді (через readability) до testset\pos & testset\neg
prepare_db.py
sparql.zip - результати виконання запитів

Стараємося поменше читати тестові дані: а) щоб не сміятися б) щоб не червоніти. Ніколи не можна вибирати фільм про Попелюшку для родинного перегляду, не перевіривши сюжет!
Щоб збалансувати тестову вибірку по кількості pos/neg прикладів, частину файлів з neg вибірки видаляємо. Тепер тестові дані містять 41 positive приклад та 29 negative.

## Перевірка фактів зі статей
check_movie.py
check_movie_test.py
result.txt

З врахуванням року випуску фільму
true+   false-
false+  true-
24      17
0       29
Statistics for the classifier:          f1      0.86179 precision       1.00000 recall  0.75714
Statistics for the 'positive' category: f1      0.73846 precision       1.00000 recall  0.58537
Statistics for the 'negative' category: f1      0.77333 precision       0.63043 recall  1.00000

Без врахування року випуску фільму
true+   false-
false+  true-
33      8
0       29
Statistics for the classifier:          f1      0.93939 precision       1.00000 recall  0.88571
Statistics for the 'positive' category: f1      0.89189 precision       1.00000 recall  0.80488
Statistics for the 'negative' category: f1      0.87879 precision       0.78378 recall  1.00000 



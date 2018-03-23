```
SELECT ?person ?relates ?to
WHERE {
  ?person a foaf:Person .
  ?person dbo:relation ?to .
  ?person ?relates ?to .
  FILTER (?relates != dbo:relation). 
}
```

|person                                  |relates                                    |to                                |
|----------------------------------------|-------------------------------------------|----------------------------------|
|:Sir_Robert_Munro,_6th_Baronet          |dbpedia2:before                            | :Sir_Robert_Munro,_5th_Baronet   |
|:Alfred_Brandon_(politician)            |owl:differentFrom                          | :Alfred_Brandon_(mayor)          |
|:Thane_Campbell                         |dbpedia:ontology/child                     | :Alex_Campbell_(politician)      |
|:Nebty-tepites                          |<http://purl.org/linguistics/gold/hypernym>| :Princess                        |
|:Humphrey_Mackworth_(Parliamentarian)   |dbpedia2:after                             | :Thomas_Mackworth                |
|:Marine_Le_Pen                          |dbpedia:ontology/predecessor               | :Jean-Marie_Le_Pen               |
|:Adnan_Khairallah                       |dbpedia:ontology/president                 | :Saddam_Hussein                  |
|:Gloria_Morgan_Vanderbilt               |dbpedia:ontology/knownFor                  | :Anderson_Cooper                 |

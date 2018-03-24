# 03-nlp-data.md

## Annotation

**Process data from the NUCLE Error Corpus**

Please see [[extract-conll2014/README.md]]

## Data Acquisition

**Process Wiktionary dump**

Please see [[extract-synonyms/README.md]]

**Download and extract as separate texts all posts in an section of choice from http://forum.lvivport.com - requires web-scraping**

Please see [[lviv-forum-scraper/README.md]]

**Write a query to collect all relations from dbpedia for every individual person listed in it - requires SPARQL**

```
SELECT DISTINCT ?needle ?has ?of ?person 
WHERE {
  {?person a foaf:Person; 
   ?of ?needle}
  UNION
  {?person a foaf:Person.
   ?needle ?has ?person}
  FILTER (?needle IN (dbr:Richard_Dawkins, dbr:Ricky_Gervais))
}
```

Result: https://goo.gl/Ae7Z6s

**Download and process an arbitrary file from Common Crawl**

Please see [[common-crabl/README.md]]

**Perform initial data collection for your project.**

Please see https://github.com/k-bx/rada-git for my progress and a repo https://github.com/k-bx/rada-git-data (its "cache") dir to see some Git LFS based files there which I downloaded (some are actually DOC/DOCX which I will need to work with).

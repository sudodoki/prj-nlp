**Write a query to collect all relations from dbpedia for every individual person listed in it - requires SPARQL**

```
SELECT ?person ?relate_to ?entity
WHERE {
  ?person a foaf:Person .  
  ?person ?relate_to ?entity .  
  #FILTER (?person IN (dbr:Barack_Obama)). 
}
```
Result without filter: https://bit.ly/2IP8WUr

Filter by person Barack_Obama: https://bit.ly/2ucULW2

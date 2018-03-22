3. Write a query to collect all relations from dbpedia for every individual person listed in it - requires SPARQL


PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbr: <http://dbpedia.org/resource/>

SELECT DISTINCT ?name ?relation
WHERE {
?person dbo:relation ?relation .
?person rdfs:label ?name FILTER (lang(?name) = "en").
}
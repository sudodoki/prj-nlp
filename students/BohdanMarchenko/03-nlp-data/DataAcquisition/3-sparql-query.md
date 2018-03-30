3. Write a query to collect all relations from dbpedia for every individual person listed in it - requires SPARQL


PREFIX dbr: <http://dbpedia.org/resource/>
select * { 
  values ?person { dbr:Johnny_Cash }
  ?person ?p ?o 
}

result: https://goo.gl/hWwALM
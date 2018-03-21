from SPARQLWrapper import SPARQLWrapper, CSV

from config import persons_dir, init_dir

init_dir(persons_dir)

MAX_ROWS = 10000
MAX_REQUESTS = 10
sparql = SPARQLWrapper("http://dbpedia.org/sparql")
sparql.setReturnFormat(CSV)
query = """
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX foaf:  <http://xmlns.com/foaf/0.1/>

SELECT ?person, ?related_person
WHERE
{
   ?person a dbo:Person .
   OPTIONAL {?person dbo:relation ?related_person}
}
"""
bounds = "LIMIT {limit} OFFSET {offset}"
offset = count = 0
while True:
    if count >= MAX_REQUESTS:
        break
    final_query = query + bounds.format(limit=MAX_ROWS, offset=offset)
    sparql.setQuery(final_query)
    results = sparql.query().convert().decode()
    count += 1
    if results.count("\n") > 1:
        filename = f"persons_{offset+1}_{offset+MAX_ROWS}"
        with (persons_dir / filename).open("w+") as f:
            f.write(results)
        offset += MAX_ROWS
    else:
        break

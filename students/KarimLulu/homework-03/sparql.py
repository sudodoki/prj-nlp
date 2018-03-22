from SPARQLWrapper import SPARQLWrapper, CSV
import sys

from config import persons_dir, init_dir

MAX_ROWS = 10000
MAX_REQUESTS = 10
ENDPOINT = "http://dbpedia.org/sparql"
QUERY = """
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX foaf:  <http://xmlns.com/foaf/0.1/>

SELECT ?person, ?related_person
WHERE
{
   ?person a dbo:Person .
   OPTIONAL {?person dbo:relation ?related_person}
}
"""
BOUNDS = "LIMIT {limit} OFFSET {offset}"

def main():
    init_dir(persons_dir)
    sparql = SPARQLWrapper(ENDPOINT)
    sparql.setReturnFormat(CSV)
    offset = count = 0
    while True:
        if count >= MAX_REQUESTS:
            break
        final_query = QUERY + BOUNDS.format(limit=MAX_ROWS, offset=offset)
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
    return 0

if __name__=="__main__":
    code = main()
    sys.exit(0)
from pathlib import Path

log_fmt='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
date_fmt="%Y-%m-%d %H:%M:%S"

root_dir = Path(__file__).resolve().parent
data_dir = root_dir / "data"
train_dir = data_dir / "train"
test_dir = data_dir / "test"
db_filename = "db.json"

# SPARQL
ENDPOINT = "http://dbpedia.org/sparql"
QUERY = """
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX dbr: <http://dbpedia.org/resource/>
PREFIX purl: <http://purl.org/linguistics/gold/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT SAMPLE(STR(?name)) as ?name SAMPLE(STR(?painting)) as ?painting
WHERE {
   ?person a dbo:Person ;
          purl:hypernym dbr:Painter ;
          dbo:birthPlace ?city ;
          foaf:name ?name .
   ?city dbo:country ?country .
   ?picture dbo:author ?person ;
            foaf:name ?painting .
FILTER (lang(?painting)="en")
FILTER (lang(?name)="en")
}
GROUP BY ?person ?picture
ORDER BY ?name ?painting
"""

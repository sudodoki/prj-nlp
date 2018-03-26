query = """
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX db: <http://dbpedia.org/page/>
PREFIX foaf:  <http://xmlns.com/foaf/0.1/>
PREFIX dbr:    <http://dbpedia.org/resource/>
PREFIX purl: <http://purl.org/linguistics/gold/>

SELECT ?name, ?picture
WHERE {
   ?person a dbo:Person ;
          purl:hypernym dbr:Painter ;
          dbo:birthName ?name ;
          dbo:birthPlace ?city ;
          dbo:birthDate ?birth .
   ?city dbo:country ?country .
   ?picture dbo:author ?person .
"""
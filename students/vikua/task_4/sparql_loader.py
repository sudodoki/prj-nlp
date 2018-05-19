import argparse
import json

import pandas as pd


from SPARQLWrapper import SPARQLWrapper, JSON


def movies_query(title):
    sparql = SPARQLWrapper("http://dbpedia.org/sparql")

    query = 'SELECT DISTINCT ?f WHERE { ?f a dbo:Film . ?f rdfs:label ?l . filter ( '
    query += 'regex(?l, "{}")'.format(title)
    query += ' ) } '
    sparql.setQuery(query)

    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()

    resources = []
    for result in results["results"]["bindings"]:
        resources.append(result['f']['value'])

    return resources


def cast_query(movie_entity):
    sparql = SPARQLWrapper("http://dbpedia.org/sparql")

    query = 'SELECT ?name WHERE { ?p a dbo:Person . ?p foaf:name ?name . '
    query += '<{}>'.format(movie_entity)
    query += ' dbo:starring ?p . }'
    sparql.setQuery(query)

    sparql.setReturnFormat(JSON)
    results = sparql.query().convert()

    cast = []
    for result in results['results']['bindings']:
        cast.append(result['name']['value'])
    return cast


def load_movies_entities(input_file, output_file, n):
    df = pd.read_csv(input_file).head(int(n))

    result = dict()
    for i, t in enumerate(df['title'].values):
        result[t] = movies_query(t)
        print('... Queried {} ...'.format(i), end='\r', flush=True)

    with open(output_file, 'w') as f:
        f.write(json.dumps(result, indent=4, sort_keys=True))


def load_casts(input_file, output_file):
    with open(input_file, 'r') as f:
        movies = json.loads(f.read())

    movies_casts = dict()
    for i, (m, v) in enumerate(movies.items()):
        cast = cast_query(v[0])
        if cast:
            movies_casts[m] = {
                'cast': cast,
                'dbpedia_urls': v[0]
            }

        print('... Queried {} ...'.format(i), end='\r', flush=True)

    with open(output_file, 'w') as f:
        f.write(json.dumps(movies_casts, indent=4, sort_keys=True))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Loading films cast from DBPedia')
    parser.add_argument('-m', dest='mode', help='Loading mode',
                        choices=['movies', 'casts'], default='movies')
    parser.add_argument('-i', dest='input_file', help='File with film titles')
    parser.add_argument('-o', dest='output_file', help='Output file')
    parser.add_argument('-n', dest='n', help='Number of films to fetch', required=False)

    args = parser.parse_args()

    if args.mode == 'movies':
        load_movies_entities(args.input_file, args.output_file, args.n)
    elif args.mode == 'casts':
        load_casts(args.input_file, args.output_file)

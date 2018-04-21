import os
import argparse

import pandas as pd

import pyspark.sql.functions as F
from pyspark.sql.types import *
from pyspark.sql import SparkSession


def load_imdb(spark, data_path, output_dir, save=False):
    titles = spark.read.csv(os.path.join(data_path, 'title.basics.tsv'), sep='\t', header=True, inferSchema=True)
    rating = spark.read.csv(os.path.join(data_path, 'title.ratings.tsv'), sep='\t', header=True, inferSchema=True)

    max_votes = rating.select(F.max(rating.numVotes).alias('max')).collect()[0].max
    min_votes = rating.select(F.min(rating.numVotes).alias('min')).collect()[0].min

    rating = rating.withColumn('votes_scaled', (rating.numVotes - min_votes) / (max_votes/10))\
        .withColumn('rating', F.col('votes_scaled') * F.col('averageRating'))

    titles = titles.where('titleType = "movie" and startYear > 1990').alias('t')\
        .join(rating.alias('r'), titles.tconst == rating.tconst, 'inner')\
        .select(F.col('t.*'), rating.rating, rating.averageRating)\
        .where(rating.averageRating > 7)

    if save:
        titles.write.csv(output_dir, header=True, sep='\t')

    return titles


def filter_top_n_films(imdb_df, n):
    def filter_genres(genres):
        g = genres.lower()
        return 'documentary' not in g and 'animation' not in g

    filter_genres_udf = F.udf(filter_genres, BooleanType())

    titles = imdb_df.where(filter_genres_udf(F.col('genres'))) \
        .sort(F.col('rating').desc()) \
        .select('primaryTitle', 'startYear').limit(n) \
        .collect()

    return [(x.primaryTitle, x.startYear) for x in titles]


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Pyspark script to fetch top movies')
    parser.add_argument('--save_imdb', dest='save_imdb', action='store_true', default=False,
                        help='Whether to preprocess and save IMDB files')
    parser.add_argument('--data_dir', dest='data_dir',
                        help='Path to directory with IMDB data')
    parser.add_argument('--output_dir', dest='output_dir',
                        help='Path to directory where top movies will be saved')
    parser.add_argument('--n', dest='n', help='Movies to fetch')

    args = parser.parse_args()

    spark = SparkSession.builder \
        .master("local") \
        .appName("Data Load") \
        .getOrCreate()

    imdb_path = os.path.join(args.output_dir, 'imdb_processed')

    imdb_df = load_imdb(spark, args.data_dir, output_dir=imdb_path, save=args.save_imdb)

    titles = filter_top_n_films(imdb_df, int(args.n))
    out = os.path.join(args.output_dir, 'top_n.csv')
    pd.DataFrame({'title': [x[0] for x in titles],
                  'year': [x[1] for x in titles]}).to_csv(out, index=False)
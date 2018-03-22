import re

from readability import Document
from langdetect import detect
from langdetect.lang_detect_exception import LangDetectException

from pyspark.sql.functions import udf, col
from pyspark.sql.types import ArrayType, StringType
from pyspark.ml.feature import Tokenizer

TAG_RE = re.compile(r'<[^>]+>')


def extract_title_and_summary(content):
    """ Apply readability to extract title and summary of the page
    """
    doc = Document(content)
    return [doc.title(), doc.summary()]


def clean_summary(summary):
    """ Delete html tags, which still present after readability processing.
    """
    if summary:
        s = TAG_RE.sub('', summary)
        s = s.replace('\n', ' ').replace('\r', '')
        s = s.replace('"', '')
        return s
    else:
        return None


def get_lang(title):
    """ Detect language from title
    """
    try:
        lang = detect(title)
    except LangDetectException:
        lang = 'unknown'
    return lang


def filter_empty_and_join(words):
    """ Filters empty tokens and joins to single string
    """
    w = [x for x in words if x.strip() != '']
    return ' '.join(w)


extract_title_and_summary_udf = udf(extract_title_and_summary, ArrayType(StringType()))
clean_summary_udf = udf(clean_summary, StringType())
get_lang_udf = udf(get_lang, StringType())
filter_empty_udf = udf(filter_empty_and_join, StringType())

df = spark.read.option("header", "true").csv("raw.csv")

df = df.withColumn('title_sum', extract_title_and_summary_udf(col("content")))\
       .withColumn('title', col('title_sum').getItem(0))\
       .withColumn('summary', col('title_sum').getItem(1))\
       .withColumn('summary', clean_summary_udf(col('summary')))\
       .withColumn('lang', get_lang_udf(col('title')))\
       .select("domain", "lang", "title", "summary")

tokenizer = Tokenizer(inputCol='summary', outputCol='summary_words')
df = tokenizer.transform(df)\
       .withColumn('words', filter_empty_udf(col('summary_words')))\
       .select('domain', 'lang', 'title', 'words')


df.write.csv("preprocessed.csv")

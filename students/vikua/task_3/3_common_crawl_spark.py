import re
import sys

from readability import Document
from langdetect import detect
from langdetect.lang_detect_exception import LangDetectException

from pyspark.sql.functions import udf, col
from pyspark.sql.types import ArrayType, StringType


TAG_RE = re.compile(r'<[^>]+>')


def extract_title_and_summary(content):
    doc = Document(content)
    return [doc.title(), doc.summary()]


def clean_summary(summary):
    if summary:
        s = TAG_RE.sub('', summary)
        s = s.replace('\n', ' ').replace('\r', '')
        s = s.replace('"', '')
        return s
    else:
        return None


def get_lang(title):
    try:
        lang = detect(title)
    except LangDetectException:
        lang = 'unknown'
    return lang


extract_title_and_summary_udf = udf(extract_title_and_summary, ArrayType(StringType()))
clean_summary_udf = udf(clean_summary, StringType())
get_lang_udf = udf(get_lang, StringType())

df = spark.read.option("header", "true").csv("raw.csv")

df = df.withColumn('title_sum', extract_title_and_summary_udf(col("content")))\
       .withColumn('title', col('title_sum').getItem(0))\
       .withColumn('summary', col('title_sum').getItem(1))\
       .withColumn('summary', clean_summary_udf(col('summary')))\
       .withColumn('lang', get_lang_udf(col('title')))\
       .select("domain", "lang", "title", "summary")

df.write.option("header", "true").csv("preprocessed.csv")

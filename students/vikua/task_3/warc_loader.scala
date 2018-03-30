import org.warcbase.spark.matchbox._
import org.warcbase.spark.rdd.RecordRDD._
import org.apache.spark.sql.functions._

val rdd = RecordLoader.loadWarc("CC-NEWS-20180228202022-00305.warc.gz", sc).
    map(elem => elem.getDomain -> elem.getContentString)

val replaceQuotesAndEOL = udf { str: String => str.replaceAll("(\r\n)|\r|\n", "").replaceAll("\"", "") }

val df = rdd.toDF().
    withColumnRenamed("_1", "domain").
    withColumnRenamed("_2", "content").
    withColumn("content", replaceQuotesAndEOL(col("content")))

df.write.option("header", "true").csv("raw.csv")

System.exit(0)
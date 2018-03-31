## How to run 

### Prerequisites

- Python 3.x
- virtualenv (and virtualenvwrapper preferably)
- [Apache Spark 2.x.x](https://spark.apache.org/downloads.html)

### Steps

1. Download and install Apache Spark (required only for Common crawl task)
2. Create and activate virtual environment (using virtualenvwrapper)
    ```
    mkvirtualenv myenv
    workon myenv
    ```

3. Install dependencies and download spacy model. 
    Note `path-to-dir-with-project` should be replaced with your local path.
    ```
    cd {path-to-dir-with-project}/prj-nlp/students/vikua
    pip install -r requirements.txt
    python -m spacy download en

#### Task 3

- Run task NUCLE error analysis
    ```
    python 3_nucle_error.py -i ../../nucledataset/noalt/ -o out
    ```

- Run cswiktionary dump parsing and synonyms extraction
    Download `s3://vikua-wiki/cswiktionary-20180301-pages-articles-multistream.xml` file
    and run
    ```
    aws s3 cp s3://vikua-wiki/cswiktionary-20180301-pages-articles-multistream.xml ../path/to/cswiktionary.xml --no-sign-request
    python 3_cswiktionary_parser.py -i ../path/to/cswiktionary.xml -o synonyms.txt
    ```

- Run lviv forum crawler and save file to local file system: 
    ```
    scrapy runspider 3_forum_crawling.py -o lviv.json
    ```
    Output file can also be downloaded [here](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_3/lviv.json)
    
- Run common crawl task 

    Download news file and warcio jar from S3 
    ```
    aws s3 cp s3://commoncrawl/crawl-data/CC-NEWS/2018/02/CC-NEWS-20180228202022-00305.warc.gz ./students/vikua/task_3/ --no-sign-request
    aws s3 cp s3://vikua-wiki/warcbase-core-0.1.0-SNAPSHOT-fatjar.jar ./students/vikua/task_3/ --no-sign-request
    ```
    Run pre-processing scala spark code. Using custom hadoop input format for warc files is faster 
    then using warcio python lib:
    ```
    spark-shell -i warc_loader.scala --jars /path/to/warcbase-core-0.1.0-SNAPSHOT-fatjar.jar
    ```
    This command will create `raw.csv` directory with crawled data. 
    
    Run script to infer language, run readability algorithm and extract title and text.
    Note, this script takes a lot of time to finish. (I think it is better to do it in pyspark)
    ```
    python3 3_common_crawl.py -i path/to/raw.csv/part-*.csv -o common_crawl.csv
    ```
    
    Analysis is done in common_crawl.ipynb notebook
    
## How to run 

### Prerequisites

- Python 3.x
- virtualenv (and virtualenvwrapper preferably)

### Steps

1. Create and activate virtual environment (using virtualenvwrapper)
    ```
    mkvirtualenv myenv
    workon myenv
    ```

2. Install dependencies and download spacy model. 
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
    Download [cswiktionary](https://s3.eu-central-1.amazonaws.com/vikua-wiki/cswiktionary-20180301-pages-articles-multistream.xml) file
    ```
    python 3_cswiktionary_parser.py -i ../path/to/cswiktionary.xml -o synonyms.txt
    ```

- Run lviv forum crawler and save file to local file system: 
    ```
    scrapy runspider 3_forum_crawling.py -o lviv.json
    ```
    Output file can also be downloaded [here](https://s3.eu-central-1.amazonaws.com/vikua-wiki/task_3/lviv.json)
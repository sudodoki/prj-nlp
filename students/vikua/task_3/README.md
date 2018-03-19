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
    ```
    python 3_cswiktionary_parser.py -i ../path/to/cswiktionary.xml -o synonyms.txt
    ```
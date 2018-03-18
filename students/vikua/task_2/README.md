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
    ```
#### Task 2
- Run task 2.1 Headlines
    ```
    python 2_headlines_solution.py -i ../../../tasks/02-structural-linguistics/examiner-headlines.txt -o headlines-formatting-output.txt
    output: Number of correct headlines 517
    ```

- Run task 2.2 Catchy headlines
    ```
    python 2_catchy_headlines_solution.py -i ../../../tasks/02-structural-linguistics/examiner-headlines.txt -s SentiWordNet_3.0.0.txt -o catchy-headlines.txt
    ```
    Output file format is kind of CSV with `|` as delimiter (`|` separated values:)) with following columns<br>
    - _text_ column with original text from input file;
    - _prominence_ column with number of prominent entities (people, nationalities,
    organizations, countries, other locations, products or events);
    - _sentiment_ column, which takes values like `positive:{positive_score}`, `negative:{negative score}`, 
    `objective:{positive_score}:{negative_score}`. <br><br>
    `positive_score` is computed as average sentiment for top 5 word+POS combinations, <br>
    `negative_score` is computed as average sentiment for botton 5 word+POS combinations
    - _superlativeness_ column, which is a number of comparative or superlative adjectives and adverbs divided by overall number of word.
    
- Run task 3 Collocations
    ```
    python 3_collocations_solution.py -i ../../../tasks/02-structural-linguistics/blog2008.txt -o collocations.txt
    ```
    Note, this task starts several processes and anyway takes a while to complete :)
    
- Run task 4 Shmification
    ```
    python 4_shmification.py -i "apple"
    Result: shmapple

    python 4_shmification.py -i "Data Science"
    Result: Data Shmience
    
    python 4_shmification.py -i "shmaltz" 
    Result: shmaltz
    
    python 4_shmification.py -i "Ashmont"
    Result: Smashmont
    ```
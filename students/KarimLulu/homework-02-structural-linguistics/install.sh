#!/bin/bash
python3.6 -m venv --without-pip hw1
source hw1/bin/activate
curl https://bootstrap.pypa.io/get-pip.py | python

pip install -r requirements.txt
python -m spacy download en
python -c "import nltk; nltk.download('sentiwordnet'); nltk.download('wordnet')"
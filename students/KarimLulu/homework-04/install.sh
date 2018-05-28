#!/bin/bash
python3.6 -m venv --without-pip hw4
source hw4/bin/activate
curl https://bootstrap.pypa.io/get-pip.py | python

pip install -r requirements.txt
python -m spacy download en
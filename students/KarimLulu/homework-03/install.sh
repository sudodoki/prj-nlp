#!/bin/bash
python3.6 -m venv --without-pip hw3
source hw3/bin/activate
curl https://bootstrap.pypa.io/get-pip.py | python

pip install -r requirements.txt

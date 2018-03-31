#!/bin/bash
python3.6 -m venv --without-pip hw5
source hw5/bin/activate
curl https://bootstrap.pypa.io/get-pip.py | python

pip install -r requirements.txt

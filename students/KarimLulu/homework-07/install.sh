#!/bin/bash
python3.6 -m venv --without-pip hw7
source hw7/bin/activate
curl https://bootstrap.pypa.io/get-pip.py | python
easy_install pip

pip install -r requirements.txt

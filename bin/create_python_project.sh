#!/bin/bash

mkdir $1_proj
cd $1_proj
touch CHANGELOG.rst
touch LICENCE
touch MANIFEST.in
touch README.rst
cat <<EOF > setup.py
from setuptools import setup
setup()
EOF
cat <<EOF > setup.cfg
[metadata]
name = $1
version = 0.1.0

[options]
packages = find:
EOF
mkdir src
cd src
mkdir $1
cd $1
touch __init__.py
cd ../../
mkdir tests
cd tests
touch __init__.py
touch conftest.py
touch pytest.ini
cd ../
touch .gitignore
git init >> /dev/null

echo "Created $1 project"

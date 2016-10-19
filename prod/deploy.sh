#!/bin/bash

SCRIPT_DIR=$(dirname $(readlink -f "$BASH_SOURCE"))

rm -rf dist
elm-make src/NoteMiner.elm --warn --output=dist/noteminer.js
cp prod/index.html dist
cd dist
git init
git add .
git commit -m "Deploy to Github Pages"
git push --force git@github.com:cbenz/noteminer.git master:gh-pages
#!/bin/sh

git clone -b gh-pages https://github.com/tlverse/pitt2019-workshop book-output
cd book-output
cp -r ../_book/* ./
git add --all *
git commit -m "Update workshop book from local build" || true
git push origin gh-pages
cd ../
rm -rf book-output


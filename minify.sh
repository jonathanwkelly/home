#!/bin/sh

java -jar ./lib/htmlcompressor.jar \
  --compress-css \
  --compress-js \
  --js-compressor=closure \
  --closure-opt-level=advanced \
  --preserve=./preserve.txt \
  --remove-surrounding-spaces=max \
  --remove-intertag-spaces \
  --remove-quotes \
  --recursive \
  --output=_site/ \
  _site/
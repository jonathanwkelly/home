htmlcompressor \
  --compress-css \
  --compress-js \
  --js-compressor=closure \
  --closure-opt-level=advanced \
  --remove-surrounding-spaces=max \
  --remove-intertag-spaces \
  --remove-quotes \
  --recursive \
  --output=_site/ \
  _site/ \
;

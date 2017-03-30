#!/bin/bash
mkdir -p tmp

INPUT_DIR=$(pwd)/inputs
OUTPUT_DIR=$(pwd)/outputs
TMP_DIR=$(pwd)/tmp
cd ..

for input in $(ls $INPUT_DIR)
do
  tmp=$TMP_DIR/${input/%.in/.tmp}
  runhaskell RunAuto.hs $INPUT_DIR/$input > $tmp
  diff $OUTPUT_DIR/${input/%.in/.out} $tmp
  if [ $? -eq 0 ]
  then
    : # echo "$tmp OK!"
  else
    echo "$tmp FAILED!"
  fi
done

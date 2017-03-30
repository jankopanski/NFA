#!/bin/bash

cd inputs

for file in `ls`; do
  line=`tail -1 $file`
  if [[ ! $line =~ [^[:space:]] ]] ; then
    echo $file
    rm $file
  fi
done

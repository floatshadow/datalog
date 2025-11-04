#!/usr/bin/env bash


for file in test/*.dl
do
  echo "Testing $file"
  moon run src/bin/top.mbt -q -g -- $file > ${file%.dl}.ans

  ret=$?
  if [ $ret -eq 0 ]; then
    echo
    echo "Test $file succeeded"
  else
    echo "Test $file failed"
  fi
done

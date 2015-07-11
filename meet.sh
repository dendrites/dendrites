#!/bin/bash
echo "ready" > $2
cat $1 > /dev/null &
wait

while true; do
  read line < $1
  echo "got $line"
done &
echo "greet.sh bump" > $2


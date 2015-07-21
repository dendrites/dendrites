#!/bin/bash
while true; do
  read line < $1
  echo "got $line"
done &
echo "greet.sh bump" > $2


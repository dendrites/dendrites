#!/bin/bash
while true; do
  read line < $1
  cmds=( $line )
  src=${cmds[0]}
  msg=${cmds[1]}
  echo "He said $msg"
done &
echo "greet.sh bump" > $2


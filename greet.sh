#!/bin/bash
echo "" > $2
while true; do
  read line < $1
  cmds=( $line )
  src=${cmds[0]}
  msg=${cmds[1]}
  echo "$src hello" > $2
done &


#!/bin/bash
echo "ready" > $2
cat $1 > /dev/null &
wait

while true; do
  read line < $1
  cmds=( $line )
  src=${cmds[0]}
  msg=${cmds[1]}
  echo "$src hello" > $2
done &


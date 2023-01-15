#!/bin/bash

SERVER=$1
PORT=8444
"$SERVER" . -p $PORT --cache --ssl domain.crt domain.key > /dev/null &
PID=$!

# need to sleep to make sure server is ready before first curl
sleep 2

url=https://localhost:${PORT}/foo_50
nb=$2

for (( c=1; c<=$nb; c++ )); do
  f=$(mktemp)
  (curl -k -s $url > $f; stat -c %s $f; rm $f) &
  PIDS[$c]=$!
done

#echo ${PIDS[@]}
wait ${PIDS[@]}

kill $PID

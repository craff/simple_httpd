#!/bin/bash

SERVER=$1
PORT=8088
"$SERVER" . -p $PORT --log-requests 0 &
PID=$!

# need to sleep to make sure server is ready before first curl
sleep 2

url=http://localhost:${PORT}/foo_50
nb=$2

for (( c=1; c<=$nb; c++ )); do
  f=$(mktemp)
  (curl -s $url > $f; stat -c %s $f; diff $f foo_50; rm $f) &
  PIDS[$c]=$!
done

#echo ${PIDS[@]}
wait ${PIDS[@]}

kill $PID

#!/bin/bash

SERVER=$1
PORT=8444
"$SERVER" . -p $PORT --debug 10 --ssl domain.crt domain.key > foo &
PID=$!

url=https://localhost:${PORT}/foo_50
nb=$2
sleep_time=0

for (( c=1; c<=$nb; c++ )); do
  f=$(mktemp)
  (curl -k -s $url > $f; stat -c %s $f; rm $f) &
  PIDS[$c]=$!

  sleep $sleep_time
done

#echo ${PIDS[@]}
wait ${PIDS[@]}

kill $PID

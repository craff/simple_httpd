#!/bin/bash

SERVER=$1
PORT=8444
"$SERVER" . -p $PORT --log-requests 0 --log-exceptions 0 -c 200 \
	  --ssl domain.crt domain.key &
PID=$!

# need to sleep to make sure server is ready before first curl
sleep 2

url=https://localhost:${PORT}/foo_50
nb=$2

for (( c=1; c<=$nb; c++ )); do
  f=$(mktemp)
  (curl -k -s $url > $f; stat -c %s $f; diff $f foo_50; rm $f) &
  PIDS[$c]=$!
done

#echo ${PIDS[@]}
wait ${PIDS[@]}

kill $PID

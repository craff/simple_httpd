#!/bin/bash

SERVER=$1
PORT=8090
"$SERVER" -p $PORT --timeout 2 --log-requests 0 --log-exceptions 0 --log-processes 0 &
PID=$!

# need to sleep to make sure server is ready before first curl
sleep 2

url=http://localhost:${PORT}/shell/echo?coucou=
nb=$2

for (( c=1; c<=$nb; c++ )); do
  f=$(mktemp)
  (curl -s $url > $f; stat -c %s $f; rm $f) &
  PIDS[$c]=$!
done

#echo ${PIDS[@]}
wait ${PIDS[@]}

sleep 1

kill $PID
wait $PID

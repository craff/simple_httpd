#!/bin/bash

SERVER=$1
NB=$2
MODE=$3
case "$MODE" in
    SSL)
	OPTIONS='--ssl domain.crt domain.key'
        PROTO='https'
	PORT=8445 ;;
    KTLS)
        OPTIONS='--ssl domain.crt domain.key --ssl-ktls'
        PROTO='https'
        PORT=8446 ;;
    *)
	OPTIONS=''
        PROTO='http'
        PORT=8088 ;;
esac

"$SERVER" ./files -p $PORT --log-requests 0 --log-exceptions 0 -c 200 $OPTIONS &
PID=$!

# need to sleep to make sure server is ready before first curl
sleep 2

url=${PROTO}://localhost:${PORT}/foo_50M

for (( c=1; c<=$NB; c++ )); do
  f=$(mktemp)
  (curl -k -s $url > $f; stat -c %s $f; diff $f files/foo_50M; rm $f) &
  PIDS[$c]=$!
done

#echo ${PIDS[@]}
wait ${PIDS[@]}

kill $PID

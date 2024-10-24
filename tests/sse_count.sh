#!/usr/bin/env sh

SSE_SERVER=$1
PORT=8086

"$SSE_SERVER" -p $PORT --log-requests 0 --log-exceptions 0 &
PID=$!
sleep 0.1

curl -N "http://localhost:${PORT}/count/10" -H user-agent:test --max-time 10
kill $PID

#!/usr/bin/env bash

PORT=8082

./sse_server.exe -p $PORT --log-requests 0 --log-exceptions 0 &
PID=$!

sleep 0.1
./sse_client.exe -p $PORT --alarm=1 /count | tr -d '\r' || true

kill $PID
echo "success"

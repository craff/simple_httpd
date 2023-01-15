#!/usr/bin/env sh

if [ -f data ]; then rm data ; fi

SERVER=$1
PORT=8087

"$SERVER" . -p $PORT --upload --max-upload 100000000000 &
PID=$!

sleep 0.1

echo download1 1>&2
curl -N "http://localhost:${PORT}/foo_50" -o data \
  -H 'Tranfer-encoding: chunked' --max-time 10

wc -c data
wc -c data 1>&2

echo download2 1>&2
curl -N "http://localhost:${PORT}/foo_50" -o data \
  -H 'Tranfer-encoding: chunked' --max-time 10

wc -c data

echo download3 1>&2
curl -N "http://localhost:${PORT}/foo_50" -o data \
  -H 'Tranfer-encoding: chunked' --max-time 10

wc -c data

echo upload1 1>&2
cat foo_50 | curl -N -X PUT http://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 >\dev\null

wc -c data

echo upload2 1>&2
cat foo_50 | curl -N -X PUT http://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 >\dev\null

wc -c data

echo upload3 1>&2
cat foo_50 | curl -N -X PUT http://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 >\dev\null

wc -c data

kill $PID

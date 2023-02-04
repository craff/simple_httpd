#!/usr/bin/env sh

if [ -f data ]; then rm data ; fi

SERVER=$1
PORT=8443

"$SERVER" . -p $PORT --ssl domain.crt domain.key --upload --max-upload 100000000000 &
PID=$!

sleep 0.1

echo download1 1>&2
curl -k -N "https://localhost:${PORT}/foo_50" -o data \
  -H 'Tranfer-encoding: chunked' --max-time 10

wc -c data
rm data

echo download2 1>&2
curl -k -N "https://localhost:${PORT}/foo_50" -o data \
  -H 'Tranfer-encoding: chunked' --max-time 10

wc -c data
rm data

echo download3 1>&2
curl -k -N "https://localhost:${PORT}/foo_50" -o data \
  -H 'Tranfer-encoding: chunked' --max-time 10

wc -c data
rm data

echo upload1 1>&2
cat foo_50 | curl -k -N -X PUT https://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 >\dev\null

wc -c data
rm data

echo upload2 1>&2
cat foo_50 | curl -k -N -X PUT https://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 >\dev\null

wc -c data
rm data

echo upload3 1>&2
cat foo_50 | curl -k -N -X PUT https://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 >\dev\null

wc -c data
rm data

kill $PID

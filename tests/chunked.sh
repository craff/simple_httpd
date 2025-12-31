#!/usr/bin/env sh

if [ -f ./store/data ]; then rm ./store/data ; fi

SERVER=$1
NB=$2
MODE=$3
case "$MODE" in
    SSL)
	OPTIONS='--ssl domain.crt domain.key'
        PROTO='https'
	PORT=8443 ;;
    KTLS)
        OPTIONS='--ssl domain.crt domain.key --ssl-ktls'
        PROTO='https'
        PORT=8444 ;;
    *)
	OPTIONS=''
        PROTO='http'
	PORT=8087
esac


"$SERVER" . -p $PORT --log-requests 0 --log-exceptions 0 --upload \
	  --max-upload 100M -j 1 --timeout 10 --dir ./store $OPTIONS &
PID=$!

sleep 0.1

echo download1 1>&2
curl -N "${PROTO}://localhost:${PORT}/foo_50M" -o ./store/data \
  -H 'Accept-Encoding: chunked' --max-time 10

wc -c ./store/data
rm ./store/data

echo download2 1>&2
curl -N "${PROTO}://localhost:${PORT}/foo_50M" -o ./store/data \
  -H 'Accept-Encoding: chunked' --max-time 10

wc -c ./store/data
rm ./store/data

echo download3 1>&2
curl -N "${PROTO}://localhost:${PORT}/foo_50M" -o ./store/data \
  -H 'Accept-Encoding: chunked' --max-time 10

wc -c ./store/data
rm ./store/data

echo upload1 1>&2
cat files/foo_50M | curl -N -X PUT ${PROTO}://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 > /dev/null

wc -c ./store/data
rm ./store/data

echo upload2 1>&2
cat files/foo_50M | curl -N -X PUT ${PROTO}://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 > /dev/null

wc -c ./store/data
rm ./store/data

echo upload3 1>&2
cat files/foo_50M | curl -N -X PUT ${PROTO}://localhost:$PORT/data --data-binary @- \
  -H 'Transfer-Encoding: chunked' --max-time 10 > /dev/null

wc -c ./store/data
rm ./store/data

kill $PID

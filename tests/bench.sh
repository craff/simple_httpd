#!/bin/bash

ulimit -n 6500

../_build/default/src/bin/http_of_dir.exe --maxc=2100 --port=8080 --timeout 10 /var/www/nginx &
PID1=$!
../_build/default/src/bin/http_of_dir.exe --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key  --maxc=2100 --port=8443 --timeout 10 /var/www/nginx &
PID2=$1
../_build/default/tests/serve_files.exe --maxc=2100 --port=9080 --timeout 10 &
PID3=$1

if [ -f small.csv ]; then rm small.csv; fi

echo nbc, apache, nginx, simple, simple-cc, simple-ssl | tee -a small.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500; seq 600 100 1000); do
    echo -n $c "," | tee -a small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d1 http://localhost:80/nginx/foo_1k | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d1 http://localhost:7080/foo_1k | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d1 http://localhost:8080/foo_1k | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d1 http://localhost:9080/foo_1k | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d1 https://localhost:8443/foo_1k | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo | tee -a small.csv
done

if [ -f big.csv ]; then rm big.csv; fi

echo nbc, apache, nginx, simple, simple-cc, simple-ssl | tee -a big.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500; seq 600 100 1000); do
    echo -n $c "," | tee -a big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d5 http://localhost:80/nginx/foo_10M | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d5 http://localhost:7080/foo_10M | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d5 http://localhost:8080/foo_10M | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d5 http://localhost:9080/foo_10M | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d5 https://localhost:8443/foo_10M | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    #curl $PROTO://localhost:8080/status 1>&2
done

kill $PID1
kill $PID2
kill $PID3

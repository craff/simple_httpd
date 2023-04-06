#!/bin/bash

ulimit -n 6500

../_build/default/src/bin/http_of_dir.exe --cache --maxc=2100 --port=8080 --timeout 10 /var/www/nginx &
PID1=$!
../_build/default/src/bin/http_of_dir.exe --send-file --maxc=2100 --port=8081 --timeout 10 /var/www/nginx &
PID2=$!
../_build/default/src/bin/http_of_dir.exe --cache --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key  --maxc=2100 --port=8443 --timeout 10 /var/www/nginx &
PID3=$1

if [ -f small.csv ]; then rm small.csv; fi

echo nbc, apache, nginx, simple, simple-sendfile, simple-ssl | tee -a small.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500; seq 600 100 1000); do
    echo -n $c "," | tee -a small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:80/nginx/echo.sh | grep Requests/sec | awk '{print $2}') "," | tee -a small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:7080/echo.sh | grep Requests/sec | awk '{print $2}') "," | tee -a small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:8080/echo.sh | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:8081/echo.sh | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 https://localhost:8443/echo.sh | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo | tee -a small.csv
done

if [ -f big.csv ]; then rm big.csv; fi

echo nbc, apache, nginx, simple, simple-sendfile, simple-ssl | tee -a big.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500; seq 600 100 1000); do
    echo -n $c "," | tee -a big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 http://localhost:80/nginx/foo_50 | grep Requests/sec | awk '{print $2}') "," | tee -a  big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 http://localhost:7080/foo_50 | grep Requests/sec | awk '{print $2}') "," | tee -a big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 http://localhost:8080/foo_50 | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 http://localhost:8081/foo_50 | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 https://localhost:8443/foo_50 | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo | tee -a big.csv
    #curl $PROTO://localhost:8080/status 1>&2
done

kill $PID1
kill $PID2
kill $PID3

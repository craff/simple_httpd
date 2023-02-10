#!/bin/bash

ulimit -n 2048

dune exec -- ../src/bin/http_of_dir.exe --cache --maxc=2100 --port=8080 --delta 10 --timeout 10 /var/www/nginx &
PID=$!

if [ -f small.csv ]; then rm small.csv; fi

echo nbc, apache, nginx, simple | tee -a small.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500; seq 600 100 1000); do
    echo -n $c "," | tee -a small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:80/nginx/echo.sh | grep Requests/sec | awk '{print $2}') "," | tee -a small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:7080/echo.sh | grep Requests/sec | awk '{print $2}') "," | tee -a small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:8080/echo.sh | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo | tee -a small.csv
    # curl http://localhost:8080/status
done

if [ -f big.csv ]; then rm big.csv; fi

echo nbc, apache, nginx, simple | tee -a big.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500; seq 600 100 1000); do
    echo -n $c "," | tee -a big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 http://localhost:80/nginx/foo_50 | grep Requests/sec | awk '{print $2}') "," | tee -a  big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 http://localhost:7080/foo_50 | grep Requests/sec | awk '{print $2}') "," | tee -a big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d30 http://localhost:8080/foo_50 | tee /dev/fd/2 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo | tee -a big.csv
    # curl http://localhost:8080/status
done

kill $PID

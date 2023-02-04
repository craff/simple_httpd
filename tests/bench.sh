#!/bin/bash

if [ -f small.csv ]; then rm small.csv; fi

echo nbc, apache, nginx, simple >> small.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500); do
    echo -n $c "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:80/nginx/echo.sh | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:7080/echo.sh | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d2 http://localhost:8080/echo.sh | grep Requests/sec | awk '{print $2}') "," >> small.csv
    echo >> small.csv
    # curl http://localhost:8080/status
done

if [ -f big.csv ]; then rm big.csv; fi

echo nbc, apache, nginx, simple >> big.csv

for c in $(seq 5 5 20; seq 30 10 50; seq 100 50 500); do
    echo -n $c "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d10 http://localhost:80/nginx/foo_50 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d10 http://localhost:7080/foo_50 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo -n $(wrk -t5 -c$c --timeout 10 -d10 http://localhost:8080/foo_50 | grep Requests/sec | awk '{print $2}') "," >> big.csv
    echo >> big.csv
    # curl http://localhost:8080/status
done

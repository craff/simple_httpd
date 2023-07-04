#!/bin/bash

# Installation

ulimit -n 3000

dune build ../src/bin/http_of_dir.exe
dune build ../examples/echo.exe
dune build ./serve_files.exe
dune build ./bench.exe

rsync -r --delete ../_build/default/tests/files/ /var/www/nginx/
rsync ./bar.php /var/www/nginx/bar.php

#vegeta tests

duration=2m

dune exec -- ../examples/echo.exe -c 2100 --log-requests 0 &
PID=$!

sleep 1

echo 'GET http://localhost:8080/vfs/bar.html' \
	| vegeta attack -rate 1000 -duration $duration > res.bin

kill $PID

cat res.bin | vegeta report > chaml_report.txt
cat res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms] > chaml.txt
cat res.bin | vegeta plot > chaml.html

echo 'GET http://localhost/nginx/bar.php' \
	| vegeta attack -rate 1000 -duration $duration > res.bin

cat res.bin | vegeta report > php_report.txt
cat res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > php.txt
cat res.bin | vegeta plot > php.html

#wrk tests

dune exec ./bench.exe

gnuplot plot.gplot

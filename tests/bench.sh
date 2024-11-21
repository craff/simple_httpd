#!/bin/bash

# Installation

ulimit -n 3000

dune build ../src/bin/http_of_dir.exe
dune build ../examples/echo.exe
dune build ./serve_files.exe
dune build ./bench.exe

rsync -r --delete ../_build/default/tests/files/ /var/www/nginx/
rsync ./bar.php /var/www/nginx/
rsync ../examples/files/foo.html /var/www/nginx/

#vegeta tests

duration=2s

dune exec -- ../examples/echo.exe --port 8080 --log-requests 0 --log-exceptions 0 -c 2100 -j 8 &
PID=$!

sleep 1

echo "simple_httpd chaml"

echo 'GET http://localhost:8080/vfs/bar.html' \
	| vegeta -cpus 8 attack -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/chaml_report.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms] > timings/chaml.txt
cat timings/res.bin | vegeta plot > timings/chaml.html

echo "simple_httpd static"

echo 'GET http://localhost:8080/vfs/foo.html' \
	| vegeta -cpus 8 attack -rate 1000 -duration $duration > timings/res.bin

kill $PID

cat timings/res.bin | vegeta report > timings/sh_report.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms] > timings/sh.txt
cat timings/res.bin | vegeta plot > timings/sh.html

echo "apache php"

echo 'GET http://localhost:80/nginx/bar.php' \
	| vegeta -cpus 8 attack -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/apache_php_report.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/apache_php.txt
cat timings/res.bin | vegeta plot > timings/apache_php.html

echo "nginx php"

echo 'GET http://localhost:7080/bar.php' \
	| vegeta -cpus 8 attack -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/nginx_php_report.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/nginx_php.txt
cat timings/res.bin | vegeta plot > timings/nginx_php.html

echo "apache static"

echo 'GET http://localhost:80/nginx/foo.html' \
	| vegeta -cpus 8 attack -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/apache_report.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/apache.txt
cat timings/res.bin | vegeta plot > timings/apache.html

echo "nginx static"

echo 'GET http://localhost:7080/foo.html' \
	| vegeta -cpus 8 attack -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/nginx_report.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/nginx.txt
cat timings/res.bin | vegeta plot > timings/nginx.html

dune exec -- ../examples/echo.exe -c 2100 --port 8443  --log-requests 0 --log-exceptions 0 --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -j 8 &
PID=$!

sleep 1

echo "simple_httpd ssl chaml"

echo 'GET https://localhost:8443/vfs/bar.html' \
	| vegeta -cpus 8 attack -insecure -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/chaml_report_ssl.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms] > timings/chaml_ssl.txt
cat timings/res.bin | vegeta plot > timings/chaml_ssl.html

echo "simple_httpd ssl static"

echo 'GET https://localhost:8443/vfs/foo.html' \
	| vegeta -cpus 8 attack -insecure -rate 1000 -duration $duration > timings/res.bin

kill $PID

cat timings/res.bin | vegeta report > timings/sh_report_ssl.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms] > timings/sh_ssl.txt
cat timings/res.bin | vegeta plot > timings/sh_ssl.html

echo "apache ssl php"

echo 'GET https://localhost:443/nginx/bar.php' \
	| vegeta -cpus 8 attack -insecure -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/apache_php_report_ssl.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/apache_php_ssl.txt
cat timings/res.bin | vegeta plot > timings/apache_php_ssl.html

echo "nginx ssl php"

echo 'GET https://localhost:7443/bar.php' \
	| vegeta -cpus 8 attack -insecure -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/nginx_php_report_ssl.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/nginx_php_ssl.txt
cat timings/res.bin | vegeta plot > timings/nginx_php_ssl.html

echo "apache ssl static"

echo 'GET https://localhost:443/nginx/foo.html' \
	| vegeta -cpus 8 attack -insecure -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/apache_report_ssl.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/apache_ssl.txt
cat timings/res.bin | vegeta plot > timings/apache_ssl.html

echo "nginx ssl static"

echo 'GET https://localhost:7443/foo.html' \
	| vegeta -cpus 8 attack -insecure -rate 1000 -duration $duration > timings/res.bin

cat timings/res.bin | vegeta report > timings/nginx_report_ssl.txt
cat timings/res.bin | vegeta report -type=hist[100us,200us,300us,400us,500us,750us,1ms,2ms,3ms,4ms,5ms,1s,5s,10s,1m] > timings/nginx_ssl.txt
cat timings/res.bin | vegeta plot > timings/nginx_ssl.html

function csv_out() {
    echo -n $1, >> $3
    grep Latencies timings/$2 | cut -d ] -f 2  >> $3
}

echo ,min, mean, 50%, 90%, 95%, 99%, max > timings/static.csv
csv_out "simple_httpd" sh_report.txt timings/static.csv
csv_out "simple_httpd ssl" sh_report_ssl.txt timings/static.csv
csv_out "nginx" nginx_report.txt timings/static.csv
csv_out "nginx ssl" nginx_report_ssl.txt timings/static.csv
csv_out "apache" apache_report.txt timings/static.csv
csv_out "apache ssl" apache_report_ssl.txt timings/static.csv


echo ,min, mean, 50%, 90%, 95%, 99%, max > timings/dynamic.csv
csv_out "simple_httpd chaml" chaml_report.txt timings/dynamic.csv
csv_out "simple_httpd chaml ssl" chaml_report_ssl.txt timings/dynamic.csv
csv_out "nginx php" nginx_php_report.txt timings/dynamic.csv
csv_out "nginx php ssl" nginx_php_report_ssl.txt timings/dynamic.csv
csv_out "apache php" apache_php_report.txt timings/dynamic.csv
csv_out "apache php ssl" apache_php_report_ssl.txt timings/dynamic.csv

python3 plot.py

echo wrk tests

dune exec ./bench.exe

python3 plot2.py

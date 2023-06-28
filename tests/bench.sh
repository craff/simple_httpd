#!/bin/bash

ulimit -n 6500

dune build ../src/bin/http_of_dir.exe
dune build ../examples/echo.exe
dune build ../tests/serve_files.exe

rsync -r --delete ../_build/default/tests/files/ /var/www/nginx/
rsync ./bar.php /var/www/nginx/bar.php

dune exec ./bench.exe

gnuplot plot.gplot

# Changes for Simple_httpd

## Changes

Author: Christophe Raffalli
    better copying in vfs_path (no more use of Sys.command and
	do not copy if mtime says so)

Author: Christophe Raffalli
    add --exclude option to vfs_path

Author: Christophe Raffalli
	better header for caching

Author: Christophe Raffalli
    Dir: avoid redirection for index.html when dowloading folder

Author: Christophe Raffalli
	Add in `Server` and `Host` a redirect_https function

Author: Christophe Raffalli
    SSL protocol in Address is now the min version. Max version is
	the max supported version (TLS 1.3)

Author: Christophe Raffalli
    Manage in Dir module (and therefore VFS) the header If-Modified-Since

## Initial announce (tag 1.0-alpha2)

I am pleased to announce the first alpha release of Simple_httpd, available on
github and opam. It is a library to produce web server and sites.

  Documentation: https://raffalli.eu/simple_httpd/simple_httpd
  Github       : https://github.com/craff/simple_httpd

WARNING: currently we need the latest master of ocaml-ssl.
It requires Linux and OCaml 5.0, if you have this,
you can install with:
```
  opam pin add https://github.com/savonet/ocaml-ssl#master -k git
  opam pin add https://github.com/craff/simple_httpd -k git
```
And test the template site (very simple, as it is an empty shell to start
from) with
```
  cd source_dir/template
  dune exec -- ./server.exe --log-folder ./log
```
The template is also documented at https://raffalli.eu/simple_httpd/simple_httpd/template.html

It aims at

- Being simple to use and rather complete (support ssl, *chaml*: an equivalent of php,
  but in OCaml and compiled, status and statistics, authentication, cookies, ...).

- Being fast: our latencies and number of requests per seconds are very good,
  thanks to using linux epoll, eventfd, OCaml's effects and domains, ...
  The first page of the documentation shows some graphics, but here is a small
  comparison of latencies for a small 1kb file:

```
                 min        mean      50%       90%       95%      99%      max
  Simple_httpd  79.478µs 242.006µs 237.576µs 294.802µs 305.68µs 329.352µs  3.049ms
  Nginx        170.551µs 328.904µs 309.577µs 384.313µs 400.51µs 482.987µs 42.003ms
  Apaches      196.321µs 466.439µs 452.265µs 545.121µs 590.05µs 913.527µs  6.372ms
```
  And a small *chaml* (our equivalent of php) against *php-fpm* from *apache*
  and *nginx*:
```
  Simple_httpd 146.944µs 285.044µs 280.552µs 341.175µs 356.497µs 507.305µs  8.069ms
  Nginx        411.151µs 793.437µs 653.131µs 796.300µs 882.268µs     2.9ms 44.504ms
  Apache       688.765µs   2.342ms 950.647µs   1.201ms   1.321ms   5.844ms   1.171s
```
  These were obtained with vegeta at 1000 requests/s. Simple_httpd offers much
  more stable latencies under charge than nginx or apache.

  If you want your own measurments, you need to setup nginx/php on ports 7080
  and 7443, an apache/php on port 80 and 443. Then, you can run [./bench.sh]
  from the [tests] folder of the source tree. I would be happy to have
  measurments for a big server with more than 20 cores.

- Currently only linux is supported.

Help, comments, bug reports, ... would be greatly appreciated, as this is
alpha release, it is time for you to propose change in the design of the
library.

My website https://raffalli.eu and therefore simple_httpd documentation are
powered by simple_httpd (do we name this bootstrap ;-) ?

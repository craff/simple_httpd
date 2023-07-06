# Simple_httpd [![build](https://github.com/craff/simple_httpd/workflows/build/badge.svg)](https://github.com/craff/simple_httpd/actions)

Simple HTTP is library to build web server and site.  It started as a fork of
[![tiny_httpd](https://github.com/c-cube/tiny_httpd)] to experiment with OCaml
5 domain and effect to replace threads.

It provides simple routing, based on URL path, address, port or even the Host
field of the HTTP header. It can serve static files from your disk (and update
on your disk are reflected immediatly), but can also compile a directory using
a provided tool `vfs_pack`. Compression with camlzip is supported in both
cases (only deflate, but we plane to add gzip). Through `vfs_pack` you can use
`.chaml` files which is similar to `.php`, but compiled with `OCaml`.

It also supports [server-sent events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events)
([w3c](https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation))

SSL is available and we use OCaml 5.0.0, epoll, evenfd and TCP_CORK options to
get the best performance. Unfortunatly, it is Linux only, but we plan to use
kqueue to support BSD, and maybe windows later.

For small request it can handle more then 1K simultaneous connection and serve
more than 100K request per second. Some graph showing the performance is
available on the [documentation](https://raffalli.eu/simple_httpd).

Out goal is to have a library able to cover the maximum use case, but simple
to use. Like Tiny_httpd, it is
still free from all forms of `ppx`, async monads, etc. 🙃 and the dependencies
remain relatively minimal (although there are now growing).

**Note**: it can be useful to add the `jemalloc` opam package for long running
server, as it does a good job at controlling memory usage.

The basic echo server from `src/examples/minimal.ml`:

https://github.com/craff/simple_httpd/blob/c7dde278b19da562bcfd64aa0643ebcf0553bd70/examples/minimal.ml#L1-27

```ocaml
module S = Simple_httpd

let () =
  let port_ = ref 8080 in
  let j = ref 32 in
  let t = ref (Domain.recommended_domain_count ()) in
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "--debug", Arg.Int S.set_debug, " set debug level";
      "-j", Arg.Set_int j, " maximum number of connections";
      "-t", Arg.Set_int t, " number of threads/domains used";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let server = S.create ~num_thread:!t ~port:!port_ ~max_connections:!j () in

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        S.Response.make_string
          (Ok (Format.asprintf "echo:@ %a@\n@." S.Request.pp req)));

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
```
```sh
$ dune exec examples/minimal.exe -j 1000 &
listening on http://127.0.0.1:8080

# the path "echo" just prints the request.
$ curl -X GET http://localhost:8080/echo --data "howdy y'all"
echo:
{meth=GET;
 headers=Host: localhost:8080
         User-Agent: curl/7.66.0
         Accept: */*
         Content-Length: 10
         Content-Type: application/x-www-form-urlencoded;
 path="/echo"; body="howdy y'all"}
$ wrk -t5 -c500 -d5 http://localhost:8080/echo
Running 5s test @ http://localhost:8081/echo
  5 threads and 500 connections
  Thread Stats   Avg      Stdev     Max   +/- Stdev
    Latency    13.37ms   11.80ms 142.23ms   85.34%
    Req/Sec     8.51k     2.52k   24.75k    69.60%
  212877 requests in 5.06s, 38.37MB read
Requests/sec:  42067.76
Transfer/sec:      7.58MB
```
Using wrk, we see the good performances: 40000 request per seconds, on
my small 6 core laptop, with the 5 client threads and 5 server threads running
on the same CPU.

## `http_of_dir`

Similar to `python -m http.server`, a simple program `http_of_dir` is provided.
It serves files from the current directory.

```sh
$ http_of_dir . -p 8080 &
$ curl -X GET http://localhost:8080
...
<html list of current dir>
...

```

## Static assets and files

The program `http_of_dir` relies on the module `Simple_httpd_dir`, which
can serve directories, as well as _virtual file systems_.

In 'examples/dune', we produce an OCaml module `vfs.ml` using
the program `tiny-httpd-vfs-pack`.  This module contains a VFS (virtual file
system) which can be served as if it were an actual directory.

The dune rule:

```lisp
(rule
  (targets vfs.ml)
  (deps (source_tree files) (:out test_output.txt.expected))
  (enabled_if (= %{system} "linux"))
  (action (run ../src/bin/vfs_pack.exe -o %{targets}
               --mirror=files/
               --file=test_out.txt,%{out}
               --url=example_dot_com,http://example.com)))
```

The code to serve the VFS from `vfs.ml` is as follows:

```ocaml
  …
  Simple_httpd_dir.add_vfs server
    ~config:(Simple_httpd_dir.config ~download:true
               ~dir_behavior:Simple_httpd_dir.Index_or_lists ())
    ~vfs:Vfs.vfs ~prefix:"vfs";
  …
```

it allows downloading the files, and listing directories.
If a directory contains `index.html` then this will be served
instead of listing the content.

## Why?

Why not? If you just want a super basic local server (perhaps for exposing
data from a local demon, like Cups or Syncthing do), no need for a ton of
dependencies or high scalability libraries.

This is a fork of the original tiny_httpd by Simon Cruanes to experiment with
OCaml 5 domains and effects. It remains simple and seems to work very well.

The plan is to try to maximise features while keeping it simple.

Use cases might include:

- serve content directly from a static blog generator;
- provide a web UI to some tool (like CUPS and syncthing do);
- implement a basic monitoring page for a service;
- provide a simple json API for a service, on top of http;
- use `http_of_dir` to serve odoc-generated docs or some assets directory.
- experimenting with domains and effects

## Documentation

See https://raffalli.eu/simple_httpd/simple_httpd

## License

MIT.

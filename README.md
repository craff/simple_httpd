# Tiny_httpd_domains [![build](https://github.com/c-cube/tiny_httpd/workflows/build/badge.svg)](https://github.com/c-cube/tiny_httpd/actions)

Minimal HTTP server, fork of
[![tiny_httpd](https://github.com/c-cube/tiny_httpd)] using OCaml 5 domain and effect,
simple routing, URL encoding/decoding, static asset serving,
and optional compression with camlzip.
It also supports [server-sent events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events)
([w3c](https://html.spec.whatwg.org/multipage/server-sent-events.html#event-stream-interpretation))

Still free from all forms of `ppx`, async monads, etc. 🙃

**Note**: it can be useful to add the `jemalloc` opam package for long running
server, as it does a good job at controlling memory usage.

The basic echo server from `src/examples/minimal.ml`:

```ocaml

module S = Tiny_httpd

let () =
  let port_ = ref 8080 in
  let j = ref 32 in
  let t = ref (Domain.recommended_domain_count ())
  Arg.parse (Arg.align [
      "--port", Arg.Set_int port_, " set port";
      "-p", Arg.Set_int port_, " set port";
      "--debug", Arg.Unit (fun () -> S._enable_debug true), " enable debug";
      "-j", Arg.Set_int j, " maximum number of connections";
      "-t", Arg.Set_int t, " number of threads/domains used";
    ]) (fun _ -> raise (Arg.Bad "")) "echo [option]*";

  let server = S.create ~num_thread:!t ~port:!port_ ~max_connections:!j () in

  (* echo request *)
  S.add_route_handler server
    S.Route.(exact "echo" @/ return)
    (fun req ->
        let q =
          S.Request.query req |> List.map (fun (k,v) -> Printf.sprintf "%S = %S" k v)
          |> String.concat ";"
        in
        S.Response.make_string
          (Ok (Format.asprintf "echo:@ %a@ (query: %s)@." S.Request.pp req q)));

  Printf.printf "listening on http://%s:%d\n%!" (S.addr server) (S.port server);
  match S.run server with
  | Ok () -> ()
  | Error e -> raise e
```sh
$ dune exec src/examples/minimal.exe &
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

```

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

The program `http_of_dir` relies on the module `Tiny_httpd_dir`, which
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
  Tiny_httpd_dir.add_vfs server
    ~config:(Tiny_httpd_dir.config ~download:true
               ~dir_behavior:Tiny_httpd_dir.Index_or_lists ())
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
OCaml 5 domain and effect. It remains tiny and seems to work very well.

Use cases might include:

- serve content directly from a static blog generator;
- provide a web UI to some tool (like CUPS and syncthing do);
- implement a basic monitoring page for a service;
- provide a simple json API for a service, on top of http;
- use `http_of_dir` to serve odoc-generated docs or some assets directory.
- experimenting with domain and effect

## Documentation

See https://c-cube.github.io/tiny_httpd_domains

## License

MIT.

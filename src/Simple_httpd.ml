
(** Simple Httpd.

    A small HTTP/1.1 server, in pure OCaml, along with some utilities
    to build small websites. It aims to be simple (not too many dependencies)
    but still provide features which would suffice for most simple websites.

    It uses domain and will treat request using a fixed number of threads
    that you can choose and no more. On each domain/thread runs a scheduler
    written with effect to treat several request concurrently. Test shows that
    it can handle hundreds of request simultaneously.
*)

module Byte_stream = Simple_httpd_stream

type listenning = Simple_httpd_domain.listenning =
  {
    addr : string;
    port : int;
    ssl  : Ssl.context option ;
  }

include Simple_httpd_server

let yield = Simple_httpd_domain.yield

let sleep = Simple_httpd_domain.sleep

module Io = Simple_httpd_domain.Io

let schedule_io = Simple_httpd_domain.schedule_io

module Mutex = Simple_httpd_domain.Mutex

module Util = Simple_httpd_util

module Dir = Simple_httpd_dir

module Html = Simple_httpd_html

module Domain = Simple_httpd_domain

module Session = Simple_httpd_session

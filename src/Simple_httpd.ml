
(** Simple Httpd.

    A small HTTP/1.1 server, in pure OCaml, along with some utilities
    to build small websites. It aims to be simple (not too many dependencies)
    but still provide features which would suffice for most simple websites.

    It uses domain and will treat request using a fixed number of threads
    that you can choose and no more. On each domain/thread runs a scheduler
    written with effect to treat several request concurrently. Test shows that
    it can handle hundreds of request simultaneously.
*)

module Util = Util

module Sfd = Util.Sfd

module Input = Input

module Output = Output

module Address = Address

module Client = Async.Client

module Async = Async

module Io = Async.Io

module Log = Async.Log

module Mutex = Async.Mutex

module Semaphore = Async.Semaphore

module Resources = Resources

module Process = Process

module Method = Method

module Headers = Headers

module Cookies = Cookies

module Camlzip = Camlzip

module Stats = Stats

module Request = Request

module Response_code = Response_code

module Response = Response

module Route = Route

module Filter = Route.Filter

module Session = Session

module Dir = Dir

module Html = Html

module Server = Server

module Auth = Auth

module Status = Status

module Host = Host

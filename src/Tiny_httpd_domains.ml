open Effect
open Effect.Deep
open Domain

type client = {
    mutable counter : int;
    mutable granularity : int;
    sock : Unix.file_descr
  }

type _ Effect.t +=
   | Read  : client * Bytes.t * int * int -> int Effect.t
   | Write : client * string  * int * int -> int Effect.t

let read  c s o l =
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0 then
    perform (Read (c,s,o,l))
  else
    Unix.read c.sock s o l

let write c s o l =
  c.counter <- c.counter + 1;
  if c.counter mod c.granularity = 0 then
    perform (Write(c,s,o,l))
  else
    Unix.single_write_substring c.sock s o l

let is_ipv6 addr = String.contains addr ':'

let connect addr port maxc =
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [Sys.sigpipe] : _ list);
  let sock =
    Unix.socket
      (if is_ipv6 addr then Unix.PF_INET6 else Unix.PF_INET)
      Unix.SOCK_STREAM
      0
  in
  Unix.set_nonblock sock;
  Unix.setsockopt_optint sock Unix.SO_LINGER None;
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.setsockopt sock Unix.SO_REUSEPORT true;
  let inet_addr = Unix.inet_addr_of_string addr in
  Unix.bind sock (Unix.ADDR_INET (inet_addr, port));
  Unix.listen sock maxc;
  sock

let loop _id addr port maxc granularity handler () =
  let listen_sock = connect addr port maxc in
  let reads = Hashtbl.create 32 in
  let writes = Hashtbl.create 32 in
  let n = ref 0 in
  let find tbl s = try (s, Hashtbl.find tbl s) with _ -> assert false in
  let get_oldest tbl t0 l =
    List.fold_left (fun ((_,(_,_,_,n,_)) as t) s' ->
        let (_, (_,_,_,n',_) as t') = find tbl s' in
        if n < n' then t else t') t0 l
  in
  let check tbl s =
    try ignore (Unix.fstat s); true with
    | Unix.Unix_error(err,_,_) ->
       Printf.eprintf "close %s\n%!" (Unix.error_message err);
       Hashtbl.remove tbl s; false
  in
  let rec select rds wrs timeout =
    try
      Unix.select rds wrs [] timeout
    with Unix.Unix_error (err,_,_) ->
      Printf.eprintf "error %s\n%!" (Unix.error_message err);
      let rds = List.filter (check reads) rds in
      let wrs = List.filter (check writes) wrs in
      select rds wrs timeout
  in
  let rec do_job () =
       let rds = listen_sock :: Hashtbl.fold (fun s _ acc -> s :: acc) reads  [] in
       let wrs = Hashtbl.fold (fun s _ acc -> s :: acc) writes [] in
       let (rds,wrs,_) = select rds wrs 300.0 in
       let last_sock = ref Unix.stdin in
       (try if List.mem listen_sock rds then begin
           Printf.eprintf "accept connection\n%!";
           let sock, _ = Unix.accept listen_sock in
           let client = { sock; counter = 0; granularity } in
           handler client; last_sock:=sock
       end else begin
         match rds, wrs with
         | (rd::rds), _ ->
            let (rd,(b,o,l,_,k)) = get_oldest reads (find reads rd) rds in
            Hashtbl.remove reads rd;
            let n = Unix.read rd b o l in
            continue k n; last_sock := rd
         | _, (wr::wrs) ->
            let (wr,(b,o,l,_,k)) = get_oldest writes (find writes wr) wrs in
            Hashtbl.remove writes wr;
            let n = Unix.single_write_substring wr b o l in
            continue k n; last_sock := wr
         | [], [] -> ()
         end
        with e ->
          Printf.eprintf "exn: %s\n%!" (Printexc.to_string e));
       (try Unix.close !last_sock with _ -> ());
       do_job ();
  in
  try_with do_job ()
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Read (rd,b,o,l) ->
           Some (fun (k : (c,_) continuation) ->
               incr n;
               Hashtbl.add reads  rd.sock (b,o,l,!n,k); do_job ())
        | Write(wr,b,o,l) ->
           Some (fun (k : (c,_) continuation) ->
               incr n;
               Hashtbl.add writes wr.sock (b,o,l,!n,k); do_job ())
        | _ -> None
    )}

let run nb addr port maxc granularity handler =
  Array.init nb (fun id -> spawn (loop id addr port maxc granularity handler))

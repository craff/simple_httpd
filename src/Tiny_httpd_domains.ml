open Effect
open Effect.Deep
open Domain

(* Queue to receive socket of accepter connection *)
let mtq = Mutex.create ()
let q   : Unix.file_descr Queue.t = Queue.create ()

let send socket =
  Mutex.lock mtq;
  Queue.add socket q;
  Mutex.unlock mtq

let receive () =
  Mutex.lock mtq;
  let r = Queue.take_opt q in
  Mutex.unlock mtq;
  r

type _ Effect.t +=
   | Read  : Unix.file_descr * Bytes.t * int * int -> int Effect.t
   | Write : Unix.file_descr * string  * int * int -> int Effect.t

let read  c s o l = perform (Read (c,s,o,l))
let write c s o l = perform (Write(c,s,o,l))

let loop _id handler () =
  let reads = Hashtbl.create 32 in
  let writes = Hashtbl.create 32 in
  let n = ref 0 in
  let find tbl s = try (s, Hashtbl.find tbl s) with _ -> assert false in
  let get_oldest tbl t0 l =
    List.fold_left (fun ((_,(_,_,_,n,_)) as t) s' ->
        let (_, (_,_,_,n',_) as t') = find tbl s' in
        if n < n' then t else t') t0 l
  in
  let rec do_job () =
    match receive () with
    | Some s -> handler s
    | None ->
       let rds = Hashtbl.fold (fun s _ acc -> s :: acc) reads  [] in
       let wrs = Hashtbl.fold (fun s _ acc -> s :: acc) writes [] in
       let (rds,wrs,_) = Unix.select rds wrs [] 0.0 in
       match rds, wrs with
       | (rd::rds), _ ->
          let (rd,(b,o,l,_,k)) = get_oldest reads (find reads rd) rds in
          Hashtbl.remove reads rd;
          (match Unix.read rd b o l with
           | n -> continue k n
           | exception Unix.Unix_error _ -> Unix.close rd; do_job ())
       | _, (wr::wrs) ->
          let (wr,(b,o,l,_,k)) = get_oldest writes (find writes wr) wrs in
          Hashtbl.remove writes wr;
          (match Unix.single_write_substring wr b o l with
           | n -> continue k n
           | exception Unix.Unix_error _ -> Unix.close wr; do_job ())
       | [], [] -> do_job ()
  in
  try_with do_job ()
    { effc = (fun (type c) (eff: c Effect.t) ->
        match eff with
        | Read (rd,b,o,l) ->
           Some (fun (k : (c,_) continuation) ->
               incr n;
               Hashtbl.add reads  rd (b,o,l,!n,k); do_job ())
        | Write(wr,b,o,l) ->
           Some (fun (k : (c,_) continuation) ->
               incr n;
               Hashtbl.add writes wr (b,o,l,!n,k); do_job ())
        | _ -> None
    )}

let run nb handler =
  Array.init nb (fun id -> spawn (loop id handler))

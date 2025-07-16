module type Resource = sig
  type t
  val create : unit -> t
  val number : int
end

module type Resources = sig
  type t
  type handle
  val get : unit -> handle * t
  val release : handle -> unit
  val use : (t -> 'a) -> 'a
end

module Make(R:Resource) = struct
  open Async

  type t = R.t
  type handle = bool ref

  (* To do: free resources not used for a long time. *)
  let lcreate () = lazy (R.create ())
  let vals = Array.init Async.max_domain
               (fun _ -> Array.init R.number (fun _ -> (ref false, lcreate())))
  let semaphore = Array.init Async.max_domain (fun _ -> Semaphore.create R.number)
  let mutex = Array.init Async.max_domain (fun _ -> Mutex.create ())

  let array_find t =
    let i = ref 0 in
    while !i < Array.length t && !(fst t.(!i)) do
      incr i
    done;
    if !i < Array.length t then t.(!i) else raise Not_found

  let get () =
    let i = (Domain.self () :> int) in
    let semaphore = semaphore.(i) in
    let mutex = mutex.(i) in
    let vals = vals.(i) in
    Semaphore.decr mutex semaphore;
    let (handle, e) =
      try
        let (handle, _) as r = array_find vals in
        handle := true;
        Mutex.unlock mutex;
        r
      with Not_found ->
        Mutex.unlock mutex;
        assert false
    in
    (handle, Lazy.force e)

  let release handle =
    let i = (Domain.self () :> int) in
    let semaphore = semaphore.(i) in
    let mutex = mutex.(i) in
    Mutex.lock mutex;
    handle := false;
    Semaphore.incr semaphore;
    Mutex.unlock mutex

  let use fn =
    let (handle, e) = get() in
    try
      let r = fn e in
      release handle;
      r
    with e ->
      release handle;
      raise e
end

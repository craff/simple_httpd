open Async

let create ?(wait_interval=0.010) cmd args : (int * Io.t) =
  let s1,s2 = Unix.(socketpair PF_UNIX SOCK_STREAM ~cloexec:false 0) in
  let pid = Unix.create_process cmd args s2 s2 s2 in
  Log.f (Prc 0) (fun k -> k "started process %s (pid: %d)" cmd pid);
  Unix.close s2; (* if we don't close the other, we do not get informed when
                    process terminates in epoll *)
  Unix.set_nonblock s1;
  let check () =
    match Unix.(waitpid [WNOHANG; WUNTRACED] pid) with
    | (p, WEXITED e) when p != 0  ->
       Log.f (Prc 0) (fun k -> k "terminaed process %s (pid: %d, exit code: %d)"
                                 cmd pid e);
       false
    | (p, _)                      ->
       Log.f (Prc 1) (fun k -> k "wait process %s (pid: %d, returns: %d)"
                                 cmd pid p);
       true
    | exception (Unix.Unix_error _ as exn) ->
       Log.f (Prc 1) (fun k -> k "wait process %s (pid: %d, raises: %s)"
                                 cmd pid (Printexc.to_string exn));
       false
  in
  let finalise () =
    while check () do
      Async.sleep wait_interval
    done;
  in
  let s1 = Io.create ~finalise s1 in
  (pid, s1)

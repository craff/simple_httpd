open Async

let create cmd args : (int * Io.t) =
  let s1,s2 = Unix.(socketpair PF_UNIX SOCK_STREAM ~cloexec:false 0) in
  Unix.set_nonblock s1;
  Unix.setsockopt_optint s1 SO_LINGER (Some 0);
  let s1 = Io.create s1 in
  let pid = Unix.create_process cmd args s2 s2 Unix.stderr in
  Unix.close s2; (* if we don't close the other, we do not get informed when
                    process terminates in epoll *)
  (pid, s1)

let rec wait ?(time=0.005) pid =
  let (pid',r) = Unix.waitpid [WNOHANG] pid in
  if pid' = 0 then (Async.sleep time; wait pid) else r

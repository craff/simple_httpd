open Async

type status = Running | Exited of int | Exn of exn

type process =
  { pid : int
  ; mutable status : status }

let create ?(wait_interval=1.0) ?stdout ?stderr ~client cmd args : (process * Io.t) =
  let s2,s1 = Unix.(socketpair PF_UNIX SOCK_STREAM ~cloexec:false 0) in
  Unix.set_close_on_exec s1;
  let stdout = match stdout with
    | None -> s2
    | Some s -> s
  in
  let stderr = match stderr with
    | None -> stdout
    | Some s -> s
  in
  let pid = Unix.create_process cmd args s2 stdout stderr in
  let proc = { pid; status = Running } in
  Log.f (Prc 0) (fun k -> k "started process %s (pid: %d, sock:%d)"
                            cmd pid (Util.file_descr_to_int s1));
  Unix.close s2; (* if we don't close the other, we do not get informed when
                    process terminates in epoll *)
  Unix.set_nonblock s1;
  let check () =
    match Unix.(waitpid [WNOHANG] pid) with
    | (_, WSTOPPED _) -> assert false
    | (0, _) ->
       Log.f (Prc 0) (fun k -> k "waiting process %s (pid: %d)" cmd pid);
       true
    | (p, WEXITED e) ->
       proc.status <- Exited e;
       Log.f (Prc 0) (fun k -> k "terminated process %s (pid: %d, returns: %d, exit code: %d)"
                                 cmd pid p e);
       false
    | (p, WSIGNALED s) ->
       Log.f (Prc 1) (fun k -> k "wait process %s (pid: %d, returns: %d, signalled: %d)"
                                 cmd pid p s);
       false
    | exception (Unix.Unix_error (code,_,_) as exn) ->
       Log.f (Prc 1) (fun k -> k "wait process %s (pid: %d, raises: %s)"
                                 cmd pid (Printexc.to_string exn));
       proc.status <- Exn exn;
       (code <> EINTR)
  in
  let finalise io =
    (try Unix.close (Io.sock io) with Unix.Unix_error _ -> ());
    while check () do
      Async.sleep wait_interval;
      (try Unix.kill Sys.sigterm pid with Unix.Unix_error _ -> ())
    done
  in
  let s1 = Io.create ~finalise ~client s1 in
  (proc, s1)

open Async

type 'a mail = { dest: string
               ; from: string
               ; subject : string
               ; action : process -> 'a
               ; cmd : string }

let mail mail fmt =
  let { dest; from; subject; action; cmd } = mail in
  let client = Client.current () in
  let (proc, inp) = create ~client
                      cmd
                      [| cmd
                       ; "-a" ; "Content-Type:text/plain;charset=utf-8"
                       ; "-a" ; "Content-Transfer-Encoding: 8bit"
                       ; "-a" ; "From: " ^ from
                       ; "-s" ; subject
                       ; dest
                      |]
  in
  let out = Io.formatter inp in
  Format.kfprintf (fun out ->
      Format.fprintf out "\n.\n%!";
      Io.close inp;
      Log.f (Exc 0) (fun k -> k "closed");
      action proc
    ) out fmt

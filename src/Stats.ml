let now = Unix.gettimeofday

(** add a missing [add_float] function to Atomic *)
module Atomic = struct
  include Atomic
  let add_float a x =
    let fn () =
      let v = Atomic.get a in
      Atomic.compare_and_set a v (v +. x)
    in
    while not (fn ()) do () done;
end

(** [Simple_httpd] provides filter for request, that can be used to collecting
    statistics. Currently, we can not count the time to output the response. *)
let filter () : 'a Route.filter * (unit -> string) =
  (* We must use atomic for this to work with domains! *)
  let nb_req     = Atomic.make 0  in
  let total_time = Atomic.make 0. in
  let parse_time = Atomic.make 0. in
  let build_time = Atomic.make 0. in

  let measure req =
    Atomic.incr nb_req;
    let t1 = Request.start_time req in
    let t2 = now () in
    (req, fun response ->
        let t3 = now () in
        Atomic.add_float total_time (t3 -. t1);
        Atomic.add_float parse_time (t2 -. t1);
        Atomic.add_float build_time (t3 -. t2);
        response)
  and get_stat () =
    let nb = Atomic.get nb_req in
    Printf.sprintf "%d requests (average response time: %.3fms = %.3fms (parse) + %.3fms (build))"
      nb (Atomic.get total_time /. float nb *. 1e3)
         (Atomic.get parse_time /. float nb *. 1e3)
         (Atomic.get build_time /. float nb *. 1e3)
  in
  (measure, get_stat)

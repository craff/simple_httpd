let now = Unix.gettimeofday

(** add missing [float] function to Atomic *)
module Atomic = struct
  include Atomic
  let add_float a x =
    let fn () =
      let v = Atomic.get a in
      Atomic.compare_and_set a v (v +. x)
    in
    while not (fn ()) do () done

  let max_float a x =
    let fn () =
      let v = Atomic.get a in
      Atomic.compare_and_set a v (max v x)
    in
    while not (fn ()) do () done
end

type stats = {
    (* We must use atomic for this to work with domains! *)
    nbreq : int Atomic.t;
    total : float Atomic.t;
    maxim : float Atomic.t;
    parse : float Atomic.t;
    build : float Atomic.t;
    send  : float Atomic.t }

let init () = {
    nbreq = Atomic.make 0;
    total = Atomic.make 0.;
    maxim = Atomic.make 0.;
    parse = Atomic.make 0.;
    build = Atomic.make 0.;
    send  = Atomic.make 0. }

let global = init ()
let per_path = Hashtbl.create 128

(** [Simple_httpd] provides filter for request, that can be used to collecting
    statistics. Currently, we can not count the time to output the response. *)
let filter () : 'a Route.Filter.t * Html.chaml =

  let measure req =
    let host = match Request.get_header req Headers.Host with
      | None -> "?"
      | Some h -> h
    in
    let path = String.concat "/" (host :: Request.path_components req) in
    let pp = try Hashtbl.find per_path path
             with Not_found -> let r = init () in
                               Hashtbl.add per_path path r;
                               r
    in
    Atomic.incr global.nbreq;
    Atomic.incr pp.nbreq;
    let t1 = Request.start_time req in
    let t2 = now () in
    (req, fun response ->
        let t3 = now () in
        Atomic.max_float global.maxim (t3 -. t1);
        Atomic.add_float global.parse (t2 -. t1);
        Atomic.add_float global.build (t3 -. t2);
        Atomic.max_float pp.maxim (t3 -. t1);
        Atomic.add_float pp.parse (t2 -. t1);
        Atomic.add_float pp.build (t3 -. t2);
        let post () =
          let t4 = now () in
          Atomic.add_float global.total (t4 -. t1);
          Atomic.add_float global.send  (t4 -. t3);
          Atomic.add_float pp.total (t4 -. t1);
          Atomic.add_float pp.send  (t4 -. t3)
        in
        Response.set_post
          (fun () -> Response.get_post response (); post ())
          response)
  in
  let stat r output =
    let nb = Atomic.get r.nbreq in
    {funml|<code>
             <?ml Out.printf "%d requests (average response time: %.3fms " nb (Atomic.get r.total /. float nb *. 1e3)?>
             <small><?ml Out.printf "%.3fms (parse) + %.3fms (build)) + %.3fms (send)"
                    (Atomic.get r.parse /. float nb *. 1e3)
                    (Atomic.get r.build /. float nb *. 1e3)
                    (Atomic.get r.send  /. float nb *. 1e3)?>
             </small>
           </code>
     |funml} output
  in
  let get_stat = {chaml|
    <!DOCTYPE html>
    <h1>Statistics of the server</h1>
    <dl><dt>global
	<dd><?ml stat global output ?>
	<?ml Hashtbl.iter (fun path r ->
	       {funml|<dt><?= path?>
                      <dd><?ml stat r output?>
               |funml} output) per_path ?>
    </dl>|chaml}
  in
  (measure, get_stat)

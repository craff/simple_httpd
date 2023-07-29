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
let filter () : 'a Route.Filter.t * (?md5_pass:Digest.t -> Html.chaml) =

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
  let stat output path r =
     let nb = Atomic.get r.nbreq in let n = float nb in
     let f x = Atomic.get x *. 1e3 /. n in
     {funml|<tr><td><?= path?>
                <td class="scol"><?= string_of_int nb?>
		<td class="scol"><?= Printf.sprintf "%.3f" (f r.total)?>
		<td class="scol"><?= Printf.sprintf "%.3f" (f r.parse)?>
		<td class="scol"><?= Printf.sprintf "%.3f" (f r.build)?>
		<td class="scol"><?= Printf.sprintf "%.3f" (f r.send)?>
	     </tr>
     |funml} output
  in
  let get_stat ?md5_pass req =
    Request.check_md5_pass md5_pass req;
    {chaml|
    <!DOCTYPE html>
    <head>
      <meta charset="UTF-8"/>
      <title>server status</title>
      <style>
           table, th, td { border: 1px solid black;
                           border-collapse: collapse; }
           table { margin-left: auto; margin-right: auto; }
           .scol { text-align: right;
                   vertical-align: top;
                   padding: 3px;
                   white-space: nowrap; }
           .info { text-align; left;
                   vertical-align: top;
                   padding: 3px; }
           .info div {
                   max-width: 75vw;
                   overflow: scroll; }
      </style>
      <script>
             function sort(tableId,index,num,desc) {
               var tbody = document.getElementById(tableId);
               var rows = Array.from(tbody.rows);

               rows.sort(function(left, right) {
                 var l = left.children[index].innerHTML;
                 var r = right.children[index].innerHTML;
                 if (desc) {
                   if (num) return (Number(r) - Number(l));
                   else return(r < l ? -1 : l < r ? 1 : 0);
                 } else {
                   if (num) return (Number(l) - Number(r));
                   else return(l < r ? -1 : r < l ? 1 : 0);
                 }
               });
               // Put them back in the tbody
               tbody.innerHTML='';
               for(var i = 0; i < rows.length; i++) {
                 tbody.appendChild(rows[i]);
               }
             };
      </script>
    </head>
    <body onload="sort('table',1,true,true);">
      <h1>Statistics of the server</h1>

      <details><summary>Note about timings</summary>
	All timings are average time in milliseconds. Parsing time does not take
	in account parsing the body of the request if it is read as a stream, in
	which case parsing time is included in building time. Similarly,
	building time of the body of the response is included in send time
	if it is a stream. Actually, if your request is build as a stream
	processer, parsing will only include the parsing of the request
	headers, build will only include the building of the response headers,
	and all the stream processing will be included in the send time.
      </details>
      <table>
        <thead><tr>
	  <th>path
	    <button onclick="sort('table',0,false,false);">▼</button>
            <button onclick="sort('table',0,false,true);">▲</button>
          <th>nb req.
            <button onclick="sort('table',1,true,false);">▼</button>
            <button onclick="sort('table',1,true,true);">▲</button>
	  <th>total
            <button onclick="sort('table',2,true,false);">▼</button>
            <button onclick="sort('table',2,true,true);">▲</button>
	  <th>parsing
            <button onclick="sort('table',3,true,false);">▼</button>
            <button onclick="sort('table',3,true,true);">▲</button>
	  <th>build
            <button onclick="sort('table',4,true,false);">▼</button>
            <button onclick="sort('table',4,true,true);">▲</button>
	  <th>send
            <button onclick="sort('table',5,true,false);">▼</button>
            <button onclick="sort('table',5,true,true);">▲</button>
	  </tr>
	  <?ml stat output "global" global ?>
	</thead>
	<tbody id="table">
	  <?ml Hashtbl.iter (stat output) per_path ?>
	</tbody>
      </table></body>|chaml} req
  in
  (measure, get_stat)

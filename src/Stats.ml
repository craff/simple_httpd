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
let graph_size = ref (24 * 60)
let graph_interval = ref 60
type graph =
  { tbl : int Atomic.t array
  ; cur : int Atomic.t
  ; mutable tim : float Atomic.t }
let graph =
  { tbl = Array.init !graph_size (fun _ -> Atomic.make 0)
  ; cur = Atomic.make 0
  ; tim = Atomic.make (Unix.gettimeofday ()) }

let add_req t1 =
  let int = float !graph_interval in
  let add_time () =
    let old = Atomic.get graph.tim in
    let new_ = old +. int in
    t1 > new_ && Atomic.compare_and_set graph.tim old new_
  in
  while add_time () do
    let old = Atomic.get graph.cur in
    let act = (old + 1) mod Array.length graph.tbl in
    if Atomic.compare_and_set graph.cur old act
    then Atomic.set graph.tbl.(act) 0
  done;
  Atomic.incr graph.tbl.(Atomic.get graph.cur)

let draw_graph size (module Output : Html.Output) =
  let i0 = Atomic.get graph.cur in
  let len = Array.length graph.tbl in
  Output.echo "<script>var data = [";
  for i = i0 - size + 1 to i0 do
    let i = if i < 0 then i + len else i in
    let n = Atomic.get graph.tbl.(i) in
    Output.printf "%d," n
  done;
  Output.echo "];</script>"

(** [Simple_httpd] provides filter for request, that can be used to collecting
    statistics. Currently, we can not count the time to output the response. *)
let filter () =
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
    add_req t1;
    let t2 = now () in
    (req, fun response ->
        let t3 = now () in
        Atomic.add_float global.parse (t2 -. t1);
        Atomic.add_float global.build (t3 -. t2);
        Atomic.add_float pp.parse (t2 -. t1);
        Atomic.add_float pp.build (t3 -. t2);
        let post () =
          let t4 = now () in
          Atomic.max_float global.maxim (t4 -. t1);
          Atomic.add_float global.total (t4 -. t1);
          Atomic.add_float global.send  (t4 -. t3);
          Atomic.max_float pp.maxim (t4 -. t1);
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
                <td class="scol"><?= Printf.sprintf "%.3f"
                               (1e3 *. Atomic.get r.maxim)?>
		<td class="scol"><?= Printf.sprintf "%.3f" (f r.parse)?>
		<td class="scol"><?= Printf.sprintf "%.3f" (f r.build)?>
		<td class="scol"><?= Printf.sprintf "%.3f" (f r.send)?>
	     </tr>
     |funml} output
  in
  let get_stat ?in_head ?in_body req headers =
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
     <?ml draw_graph !graph_size output ?>
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
       function filter(tableId,index,inputId) {
           var str = document.getElementById(inputId).value;
	   var tbody = document.getElementById(tableId);
           var rows = tbody.rows;

	   for(var i = 0; i < rows.length; i++) {
	       let inner = rows[i].cells[index].textContent;
	       if (inner.indexOf(str) >= 0) {
		   rows[i].style.display = 'table-row';
	       } else {
		   rows[i].style.display = 'none';
	       }
	   }
       };
       var zoom_graph = 1;
       function draw_graph(canvasId, data) {
	   const canvas = document.getElementById(canvasId);
	   canvas.width = window.innerWidth * 0.8;
	   canvas.height = window.innerHeight * 0.25;
	   canvas.style.marginLeft = window.innerWidth * 0.1 + "px";
	   const w = canvas.width;
	   const h = canvas.height;
           let maxi = 0;
	   let len = Math.floor(data.length * zoom_graph);
	   let start = data.length - len;
           for (i = start; i < data.length; i++) {
	       maxi = Math.max(maxi,data[i]);
	   }
    	   function X(i) { return (((i - start) * w) / (len - 1) ); };
	   function Xinv(t) { return (t * (len - 1) / w + start); };
    	   function Y(i) { return ((maxi - i) * (h - 1) / maxi); };
    	   function Yinv(t) { return (maxi - t * maxi / (h - 1)); };
	   const ctx = canvas.getContext('2d');
	   ctx.beginPath();
	   ctx.moveTo(X(0),Y(data[0]))
	   for (var i = start; i < data.length; i++) {
	       ctx.lineTo(X(i),Y(data[i]));
	   }
	   ctx.stroke();
	   ctx.font = "12px Arial";
	   ctx.setLineDash([5, 15]);
	   for (var i = 1; i <= 5; i++) {
	       ctx.fillText(Yinv(i*h/6).toFixed(2),0,i*h/6 + 5);
	       ctx.beginPath();
	       ctx.moveTo(40,i * h / 6);
	       ctx.lineTo(w,i * h / 6);
	       ctx.stroke();
	   }
       }
       function do_zoom(event) {
	   event.preventDefault();
	   let dY = event.deltaY;
 	   zoom_graph = Math.min(1,zoom_graph * Math.pow(2, event.deltaY / 250));
           draw_graph('canvas',data);
       }
       document.getElementById('canvas').addEventListener("wheel", do_zoom);
       function onLoad() {
	   sort('table',1,true,true);
	   draw_graph('canvas',data);
	   setTimeout ( "location.reload ();", 60000);
       }
     </script>
     <?ml match in_head with None -> () | Some f -> f output ?>
    </head>
    <body onload="onLoad();">
      <?ml match in_body with None -> () | Some f -> f output ?>
     <h1>Statistics of the server</h1>

     <canvas style="margin: 0 auto;" id='canvas'>
     </canvas>

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
            <input id="path_filter" type="text"
		   onchange="filter('table',0,'path_filter')">
          <th>nb req.
            <button onclick="sort('table',1,true,false);">▼</button>
            <button onclick="sort('table',1,true,true);">▲</button>
	  <th>total
            <button onclick="sort('table',2,true,false);">▼</button>
            <button onclick="sort('table',2,true,true);">▲</button>
	  <th>max
            <button onclick="sort('table',3,true,false);">▼</button>
            <button onclick="sort('table',3,true,true);">▲</button>
	  <th>parsing
            <button onclick="sort('table',4,true,false);">▼</button>
            <button onclick="sort('table',4,true,true);">▲</button>
	  <th>build
            <button onclick="sort('table',5,true,false);">▼</button>
            <button onclick="sort('table',5,true,true);">▲</button>
	  <th>send
            <button onclick="sort('table',6,true,false);">▼</button>
            <button onclick="sort('table',6,true,true);">▲</button>
	  </tr>
	  <?ml stat output "global" global ?>
	</thead>
	<tbody id="table">
	  <?ml Hashtbl.iter (stat output) per_path ?>
	</tbody>
      </table></body>|chaml} req headers
  in
  (measure, get_stat)

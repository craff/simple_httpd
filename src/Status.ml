open Server
open Log

let date time (module Out : Html.Output) =
  let date = Unix.gmtime time in
  let frac_date = mod_float time 1.0 in
  let frac_date = Printf.sprintf "%0.5f" frac_date in
  let frac_date = String.sub frac_date 2 5 in
  let frac_date = {html|<small>{`frac_date`}</small>|html} in
  Out.printf "%02d-%02d-%d %02d:%02d:%02d.%s"
           (date.tm_year+1900) (date.tm_mon + 1) date.tm_mday
           date.tm_hour date.tm_min date.tm_sec frac_date

let log_line i (time, client, rest) output =
  {funml|
   <tr>
     <td class="scol">
        <ml>date time output</ml></td>
        <td class="scol">{`string_of_int i`}</td>
        <td class="scol">{`string_of_int client`}</td>
        <td class="info">{`(rest)`}</td>
      </tr>|funml} output

let get_log client i nb_lines output =
  let filename = fname i in
  try
    let (_pid, out) =
      Process.create ~client "tail" [|"tail"; "-n"; string_of_int nb_lines; filename|]
    in
    let ch = Input.of_io out in
    let b = Buffer.create 1024 in
    let buf = Buffer.create 128 in
    let cont = ref true in
    let start line = String.length line > 0 && '0' <= line.[0] && line.[0] <= '9' in

    let first_line =
      ref (let rec fn () =
             let line = Input.read_line ~buf ch in
             if start line then line else fn ()
           in fn ())
    in
    let fn () =
      let time, client, rest =
        Scanf.sscanf !first_line "%f %d %d %n"
          (fun time _ cl rest ->
            time, cl,
            String.sub !first_line rest (String.length !first_line - rest))
      in
      Buffer.add_string b rest;
      let rec gn () =
        let line = Input.read_line ~buf ch in
        cont := not (Input.end_of_input ch);
        if String.length line > 0 && '0' <= line.[0] && line.[0] <= '9' then
          first_line := line
        else
          (Buffer.add_string b "\n"; Buffer.add_string b line; if !cont then gn ())
      in
      (try gn () with Unix.(Unix_error(EPIPE,_,_)) -> cont := false);
      log_line i (time, client, Buffer.contents b) output;
      Buffer.reset b
    in
    while !cont do fn () done;
    Input.close ch;
  with e -> log_line i (0.0, 0,
             Printf.sprintf "Can not read log file %s (exn: %s)\n%!"
               filename (Printexc.to_string e)) output

let html ?log_size ?in_head ?css ?start_header ?end_header
      ?start_contents ?end_contents self req headers =
  let log_size =
    match log_size with
    | Some nb -> nb
    | None ->
       try int_of_string (List.assoc "nb" (Request.query req)) with _ -> 100
  in
  let num_threads = num_threads self in
  let mypid = Unix.getpid () in
  let client = Request.client req in
  let ps =
    try
      let (_,out) =
        Process.create ~client"ps" [| "ps";"-p"; string_of_int mypid;"-o"
                               ; "%cpu,rss,vsz,pmem"|]
      in
      let ch = Input.of_io out in
      let buf = Buffer.create 128 in
      let _ = Input.read_line ~buf ch in
      let ps = Input.read_line ~buf ch in
      let ps =
        Scanf.sscanf ps " %f %d %d %f"
          (fun cpu rss vsz pmem  ->
            let rss = Util.to_human_int (rss * 1024) in
            let vsz = Util.to_human_int (vsz * 1024) in
            Printf.sprintf "%.2f%% CPU, %s Memory (%s resident, %.2f%%)"
              cpu vsz rss pmem)
      in
      Input.close ch;
      ps
    with _ -> "Can not measure CPU"
  in
  let df =
    try
      let (_,out) =
        Process.create ~client "df" [| "df";"-h"; "."|]
      in
      let ch = Input.of_io out in
      let buf = Buffer.create 128 in
      let _ = Input.read_line ~buf ch in
      let df = Input.read_line ~buf ch in
      Input.close ch;
      df
    with _ -> "Can not measure Disk Status"
  in
  (* TODO: use "lsof" if /proc/self/fd does not exists *)
  let nfd =
    try Array.length (Sys.readdir "/proc/self/fd")
    with _ -> -1
  in
  let css = match css with
    | None -> ""
    | Some s -> {html|<link rel="stylesheet" href={`s`}>|html}
  in
  {chaml|
   <!DOCTYPE html>
   <html>
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
           .info { text-align: left;
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
          <ml>match in_head with None -> () | Some f -> f output</ml>
          {`css`}
       </head>
       <body onload="sort('table',0,false,true);">
         <header>
           <ml>match start_header with None -> () | Some f -> f output</ml>
           <h1><ml>printf "Server status %d+1 threads" num_threads</ml></h1>
	   <ml>match end_header with None -> () | Some f -> f output</ml>
         </header>
         <div class="contents">
           <ml>match start_contents with None -> () | Some f -> f output</ml>
	   <ul>
             <li>Started at (<ml>date (started_time self) output</ml>)
             <li>{`ps`}
             <li>{`df`}
             <li>{`string_of_int nfd`} opened file descriptors
               <ml>
		for i = 0 to num_threads - 1 do
		let did = ((Server.domains self).(i) :> int) in
                let dinfo = Async.all_domain_info.((did :> int)) in
                let pps = dinfo.pendings in
                echo {html|<li>{`
                   Printf.sprintf "Thread %d: %d=%d connections" did
                                  (Util.LinkedList.size (dinfo.last_seen))
                                  (Atomic.get (dinfo.nb_connections))
                               `}|html};
               done
               </ml>
	   </ul>
	   <ml>
	    if !Log.log_folder <> "" then
	   {funml|
	   <h2>Logs</h2>
	   <table>
             <thead>
               <tr>
		 <th>date
		   <button onclick="sort('table',0,false,false);">▼</button>
		   <button onclick="sort('table',0,false,true);">▲</button>
		 <th>domain
		   <button onclick="sort('table',1,true,false);">▼</button>
		   <button onclick="sort('table',1,true,true);">▲</button>
		 <th>client
               <button onclick="sort('table',2,true,false);">▼</button>
               <button onclick="sort('table',2,true,true);">▲</button>
		 <th>information
		   <tbody id="table">
		     <ml>
		      let _ = for i = 0 to num_threads do
		      get_log client i log_size output;
		      done
		     </ml>
	   </table>
	   |funml} output
	   </ml>
	   <ml>match end_contents with None -> () | Some f -> f output</ml>
	 </div>
      </body>
   </html>|chaml} req headers

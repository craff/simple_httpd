open Server
open Log

let log_line i (time, date, client, rest) output =
  let frac_date = mod_float time 1.0 in
  let frac_date = Printf.sprintf "%0.5f" frac_date in
  let frac_date = String.sub frac_date 2 5 in
  let frac_date = {html|<small><?=frac_date?></small>|html} in
  let open Unix in
  {funml|
   <tr>
     <td class="scol">
       <?= Printf.sprintf "%02d-%02d-%d %02d:%02d:%02d.%s"
           (date.tm_year+1900) (date.tm_mon + 1) date.tm_mday
           date.tm_hour date.tm_min date.tm_sec frac_date
           ?></td>
        <td class="scol"><?= string_of_int i ?></td>
        <td class="scol"><?= string_of_int client ?></td>
        <td class="info"><?= (rest) ?></td>
      </tr>|funml} output

let get_log i nb_lines output =
  let filename = fname i in
  try
    let (pid, out) =
      Process.create "tail" [|"tail"; "-n"; string_of_int nb_lines; filename|]
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
        if String.length line > 0 && '0' <= line.[0] && line.[0] <= '9' then
          first_line := line
        else
          (Buffer.add_string b "\n"; Buffer.add_string b line; gn ())
      in
      (try gn () with Unix.(Unix_error(EPIPE,_,_)) | End_of_file -> cont := false);
      let date = Unix.gmtime time in
      log_line i (time, date, client, Buffer.contents b) output;
      Buffer.reset b
    in
    while !cont do fn () done;
    Input.close ch;
    ignore (Process.wait pid);
  with e -> log_line i (0.0, Unix.gmtime 0.0, 0,
             Printf.sprintf "Can not read log file %s (exn: %s)\n%!"
               filename (Printexc.to_string e)) output

let html ?log_size ?check ?in_head ?in_body self req headers =
  let sess_cookies =
    match check with
    | None -> Cookies.empty
    | Some f -> fst (f req)
  in
  let status = status self in
  let log_size =
    match log_size with
    | Some nb -> nb
    | None ->
       try int_of_string (List.assoc "nb" (Request.query req)) with _ -> 100
  in
  let num_threads = num_threads self in
  let mypid = Unix.getpid () in
  let (pid1,out) =
    Process.create "ps" [| "ps";"-p"; string_of_int mypid;"-o"
                         ; "%cpu,rss,vsz,pmem"|]
  in
  let ch = Input.of_io out in
  let buf = Buffer.create 128 in
  let _ = Input.read_line ~buf ch in
  let ps = Input.read_line ~buf ch in
  Input.close ch;
  let (pid2,out) =
    Process.create "df" [| "df";"-h"; "."|]
  in
  let ch = Input.of_io out in
  let buf = Buffer.create 128 in
  let _ = Input.read_line ~buf ch in
  let df = Input.read_line ~buf ch in
  Input.close ch;
  let nfd = Array.length (Sys.readdir "/proc/self/fd") in
  let ps =
    Scanf.sscanf ps " %f %d %d %f"
      (fun cpu rss vsz pmem  ->
        let rss = Util.to_human_int (rss * 1024) in
        let vsz = Util.to_human_int (vsz * 1024) in
        Printf.sprintf "%.2f%% CPU, %s Memory (%s resident, %.2f%%)"
                   cpu vsz rss pmem)
  in
  ignore (Process.wait pid1);
  ignore (Process.wait pid2);
  {chaml|
   <!DOCTYPE html>
   <?prelude let cookies = sess_cookies ?>
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
          <?ml match in_head with None -> () | Some f -> f output ?>
       </head>
       <body onload="sort('table',0,false,true);">
           <?ml match in_body with None -> () | Some f -> f output ?>
           <h1><?ml printf "Server status %d+1 threads" num_threads?></h1>
           <ol>
           <li><?= ps ?></li>
           <li><?= df ?></li>
           <li><?= string_of_int nfd ?> opened file descriptors</li>
           <?ml
             for i = 0 to num_threads do
               if i = 0 then
                 echo {html|<li>Thread <?= string_of_int i ?> accepting clients</li>|html}
               else
                 begin
                   let did = status.domain_ids.(i-1) in
                   let pps = Async.all_domain_info.((did :> int)).pendings in
                   echo {html|<li><?=
                      Printf.sprintf "Thread %d: %d=%d-1 connections (%d)" i
                                   (Atomic.get (status.nb_connections.(i-1)))
                                   (Hashtbl.length pps) (did :> int) ?></li>|html};
                 end
              done
           ?></ol>
     <?ml
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
           <?ml
             let _ = for i = 0 to num_threads do
               get_log i log_size output;
             done
           ?>
       </table>
       |funml} output
     ?>
      </body>
   </html>|chaml} req headers

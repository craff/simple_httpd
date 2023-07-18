open Server

let html ?(log_size=100) self =
  let open Html in
  let status = status self in
  let num_threads = num_threads self in
  let printf fmt = Printf.ksprintf (fun s -> txt s) fmt in
  let pid = Unix.getpid () in
  let ch = Unix.open_process_args_in "ps" [|"ps";"-p";
                                   string_of_int pid;"-o";"%cpu,rss,vsz,pmem"|]
  in
  let _ = input_line ch in
  let ps = input_line ch in
  Printf.eprintf "%S\n%!" ps;
  let ps =
    Scanf.sscanf ps " %f %d %d %f"
      (fun cpu rss vsz pmem  ->
        let rss = Util.to_human_int (rss * 1024) in
        let vsz = Util.to_human_int (vsz * 1024) in
        Printf.sprintf "%.2f%% CPU, %s Memory (%s resident, %.2f%%)"
                   cpu vsz rss pmem)
  in
  let _ = Unix.close_process_in ch in
  let log_line i acc (date, client, rest) =
    let open Unix in
    let acc = tr[][td["class","scol"][printf "%02d-%02d-%d %02d:%02d:%02d"
           (date.tm_year+1900) (date.tm_mon + 1) date.tm_mday
           date.tm_hour date.tm_min date.tm_sec] ;
           td["class","scol"][printf "%d" i] ;
           td["class","scol"][printf "%d" client] ;
           td["class","info"][div[][pre[][txt rest]]]] :: acc
    in acc
  in
  let logs =
    let logs =
      List.init (num_threads + 1)
        (fun i -> Async.Log.get_log i log_size)
    in
    table[][
        thead[][
            tr[][th[][txt "date";
                      span["onclick","sort('table',0,false,false);"][raw_html "▼"];
                      span["onclick","sort('table',0,false,true);"][raw_html "▲"]];
                 th[][txt "domain";
                      span["onclick","sort('table',1,true,false);"][raw_html "▼"];
                      span["onclick","sort('table',1,true,true);"][raw_html "▲"]];
                 th[][txt "client";
                      span["onclick","sort('table',2,true,false);"][raw_html "▼"];
                      span["onclick","sort('table',2,true,true);"][raw_html "▲"]];
                 th[][txt "information"]]] ;


        tbody["id","table"](
            fst (List.fold_left
                   (fun (acc,i) ls ->
                     (List.fold_left (log_line i)
                        acc ls, i+1)) ([],0) logs))]
  in
  html [] [
      head [] [meta["charset","UTF-8"];
               title[][txt "server status"];
               style[][txt "table, th, td { border: 1px solid black;
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
                                    overflow: scroll; }"];
               script[][raw_html
      {|
       function sort(tableId,index,num,asc) {
       var tbody = document.getElementById(tableId);
       var rows = Array.from(tbody.rows);

       rows.sort(function(left, right) {
         var l = left.children[index].innerHTML;
         var r = right.children[index].innerHTML;
         if (asc) {
           if (num) return (Number(l) - Number(r));
           else return(l < r ? -1 : r < l ? 1 : 0);
         } else {
           if (num) return (Number(r) - Number(l));
           else return(r < l ? -1 : l < r ? 1 : 0);
         }
       });

       // Put them back in the tbody
       tbody.innerHTML='';
       for(var i = 0; i < rows.length; i++) {
         tbody.appendChild(rows[i]);
       }
      };|}]];
      body ["onload","sort('table',0,false);"] [
          h1[][printf "Server status %d+1 threads - %s" num_threads ps];
          ol[](List.init (num_threads + 1) (fun i ->
                  li[][if i = 0 then
                         printf "Thread %d: accepting clients" i
                       else
                         begin
                           let did = status.domain_ids.(i-1) in
                           let pps = Async.all_domain_info.((did :> int)).pendings in
                           printf "Thread %d: %d=%d-1 connections (%d)" i
                             (Atomic.get (status.nb_connections.(i-1)))
                             (Hashtbl.length pps) (did :> int)
                           ;
                         end;]));
          h2[][txt "Logs"];
          logs
  ]]

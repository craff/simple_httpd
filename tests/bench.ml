
let add_column csv title fn data =
  let data = title :: List.map fn data in
  let nb_col =
    match csv with
    | [] -> 0
    | l::_ -> List.length l
  in
  let fill () = List.init nb_col (fun _ -> "") in
  let rec fn acc csv data =
    match (csv, data) with
    | ([], []) -> List.rev acc
    | ([], d::data) ->
       let acc = (fill () @ [d]) :: acc in
       fn acc [] data
    | (row::csv, d::data) ->
       let acc = (row @ [d]) :: acc in
       fn acc csv data
    | (row::csv, []) ->
       let acc = (row @ [""]) :: acc in
       fn acc csv []
  in
  fn [] csv data

let measure server cmd values =
  let s = match server with
    | Some server ->
       let args = Array.of_list (String.split_on_char ' ' server) in
       let chs = Unix.open_process_args args.(0) args in
       let pid = Unix.process_pid chs in
       Unix.sleep 1;
       Some(chs,pid)
    | None -> None
  in

  let fn v =
    let res = ref 0.0 in
    let ch = Unix.open_process_in (cmd v) in
    try
      while true do
        let line = input_line ch in
        Printf.eprintf "%s\n%!" line;
        (try Scanf.sscanf line "Requests/sec: %f" (fun f -> Printf.printf "%f\n%!" f; res := f)
        with _ -> ());
      done;
      assert false
    with End_of_file -> ignore (Unix.close_process_in ch); !res
  in
  let res = List.map fn values in
  (match s with
   | Some((ch,ch'),pid) ->
       Unix.kill pid Sys.sigkill;
       close_in ch; close_out ch'
   | None -> ());
  res

let nb_conn = [10;50;100;200;300;400;500;600;700;800;900;1000]

(* ======================== TEST of .chaml ====================== *)

let files = ["bar.html", 1]
let values = List.(flatten (map (fun file -> map (fun n -> (file,n)) nb_conn) files))

let csv = ref []

let _ = csv := add_column !csv "file" fst (List.map fst values)
let _ = csv := add_column !csv "nbc" string_of_int (List.map snd values)

let data =
  Printf.printf "serve_files\n%!";
  measure
    (Some "../_build/default/examples/echo.exe -c 2100")
    (fun ((file, d), c) ->
      Printf.sprintf "wrk -t5 -c%d --timeout 10 -d%d http://localhost:8080/vfs/%s"
        c d file)
    values

let _ = csv := add_column !csv "sh\\\\_cc" string_of_float data

let data =
  Printf.printf "measure apache\n%!";
  measure None
    (fun ((file, d), c) ->
      let file = Filename.remove_extension file ^ ".php" in
      Printf.sprintf "wrk -t5 -c%d --timeout 10 -d%d http://localhost:80/nginx/%s"
        c d file)
    values

let _ = csv := add_column !csv "apache" string_of_float data

let _ = Csv.save "bench_chaml.csv" !csv

(* ======================== TEST of static files ====================== *)

let files = ["foo_1k", 1; "foo_100k", 2; "foo_10M", 10]
let values = List.(flatten (map (fun file -> map (fun n -> (file,n)) nb_conn) files))

let _ = csv := []

let _ = csv := add_column !csv "file" fst (List.map fst values)
let _ = csv := add_column !csv "nbc" string_of_int (List.map snd values)

let data =
  Printf.printf "serve_files\n%!";
  measure
    (Some "../_build/default/tests/serve_files.exe --dir /var/www/nginx -c 2100 --timeout 10")
    (fun ((file, d), c) ->
      Printf.sprintf "wrk -t5 -c%d --timeout 10 -d%d http://localhost:9080/%s"
        c d file)
    values

let _ = csv := add_column !csv "sh\\\\_cc" string_of_float data

let data =
  Printf.printf "measure http_of_dir\n%!";
  measure
    (Some "../_build/default/src/bin/http_of_dir.exe -c 2100 --port=8080 --timeout 10 /var/www/nginx")
    (fun ((file, d), c) ->
      Printf.sprintf "wrk -t5 -c%d --timeout 10 -d%d http://localhost:8080/%s"
        c d file)
    values

let _ = csv := add_column !csv "sh\\\\_dir" string_of_float data

let data =
  Printf.printf "measure http_of_dir\n%!";
  measure
    (Some "../_build/default/src/bin/http_of_dir.exe --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -c 2100 --port=8443 --timeout 10 /var/www/nginx")
    (fun ((file, d), c) ->
      Printf.sprintf "wrk -t5 -c%d --timeout 10 -d%d https://localhost:8443/%s"
        c d file)
    values

let _ = csv := add_column !csv "sh\\\\_ssl" string_of_float data

let data =
  Printf.printf "measure nginx\n%!";
  measure None
    (fun ((file, d), c) ->
      Printf.sprintf "wrk -t5 -c%d --timeout 10 -d%d http://localhost:7080/%s"
        c d file)
    values

let _ = csv := add_column !csv "nginx" string_of_float data

let data =
  Printf.printf "measure apache\n%!";
  measure None
    (fun ((file, d), c) ->
      Printf.sprintf "wrk -t5 -c%d --timeout 10 -d%d http://localhost:80/nginx/%s"
        c d file)
    values

let _ = csv := add_column !csv "apache" string_of_float data


let _ = Csv.save "bench.csv" !csv

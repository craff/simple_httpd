
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

let err_regexp = Str.regexp "errors.*"

let measure server cmd values =
  let s = match server with
    | Some server ->
       let args = Array.of_list (String.split_on_char ' ' server) in
       let chs = Unix.open_process_args_out args.(0) args in
       let pid = Unix.process_out_pid chs in
       Some(chs,pid)
    | None -> None
  in

  let fn v =
    let res = ref 0.0 in
    Unix.sleepf 0.2;
    let ch = Unix.open_process_in (cmd v) in
    try
      while true do
        let line = input_line ch in
        if String.starts_with ~prefix:"requests" line ||
             String.starts_with ~prefix:"time" line
        then Printf.printf "%s\n%!" line;
        (try Scanf.sscanf line "finished in %f%[ms], %f "
               (fun _ _ f -> Printf.printf "%s\n%!" line; res := f)
        with _ -> ());
      done;
      assert false
    with End_of_file -> ignore (Unix.close_process_in ch); !res
  in
  let res = List.map fn values in
  (match s with
   | Some(ch,pid) ->
       Unix.kill pid Sys.sigkill;
       close_out ch
   | None -> ());
  res

let nb_conn = [10;50;100;200;300;400;500;600;800;1000;1200;1500;2000]

let csv = ref []

(* ======================== TEST of static files ====================== *)

let files = ["foo_1k", 50_000; "foo_50k", 10_000; "foo_500k", 2_000 ]
let values = List.(flatten (map (fun file -> map (fun n -> (file,n)) nb_conn) files))

let _ = csv := add_column !csv "file" fst (List.map fst values)
let _ = csv := add_column !csv "nbc" string_of_int (List.map snd values)

let data =
  Printf.printf "measure vfs_pack\n%!";
  measure
    (Some "../_build/default/tests/serve_files.exe --port 9080 --log-requests 0 --log-exceptions 0 --dir /var/www/nginx -c 2100 -j 8 --timeout 10")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d http://localhost:9080/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd vfs_path" string_of_float data

let data =
  Printf.printf "measure vfs_pack ssl\n%!";
  measure
    (Some "../_build/default/tests/serve_files.exe --port 9443 --log-requests 0 --log-exceptions 0 --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -c 2100 -j 8 --port=9443 --dir /var/www/nginx --timeout 10")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:9443/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd vfs_pack ssl" string_of_float data
(*
let data =
  Printf.printf "measure vfs_pack ssl+ktls\n%!";
  measure
    (Some "../_build/default/tests/serve_files.exe --log-requests 0 --ktls --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -c 2100 -j 8 --port=9444 --dir /var/www/nginx -c 2100 -j 8 --timeout 10")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:9444/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd vfs_pack ssl+ktls" string_of_float data
 *)
let data =
  Printf.printf "measure http_of_dir\n%!";
  measure
    (Some "../_build/default/src/bin/http_of_dir.exe --log-requests 0 --log-exceptions 0 -c 2100 -j 8 --port=8080 --timeout 10 /var/www/nginx")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d http://localhost:8080/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd add_dir_path" string_of_float data

let data =
  Printf.printf "measure http_of_dir ssl\n%!";
  measure
    (Some "../_build/default/src/bin/http_of_dir.exe --log-requests 0 --log-exceptions 0 --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -c 2100 -j 8 --port=8443 --timeout 10 /var/www/nginx")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:8443/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd add_dir_path ssl" string_of_float data
(*
let data =
  Printf.printf "measure http_of_dir ssl+ktls\n%!";
  measure
    (Some "../_build/default/src/bin/http_of_dir.exe --log-requests 0 --ktls --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -c 2100 -j 8 --port=8444 --timeout 10 /var/www/nginx")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:8444/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd ssl+ktls" string_of_float data
 *)
let data =
  Printf.printf "measure nginx\n%!";
  measure None
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d http://localhost:7080/%s"
        c d file)
    values

let _ = csv := add_column !csv "nginx" string_of_float data


let data =
  Printf.printf "measure nginx ssl\n%!";
  measure None
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:7443/%s"
        c d file)
    values

let _ = csv := add_column !csv "nginx ssl" string_of_float data

let data =
  Printf.printf "measure apache\n%!";
  measure None
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d http://localhost:80/nginx/%s"
        c d file)
    values

let _ = csv := add_column !csv "apache" string_of_float data

let data =
  Printf.printf "measure apache ssl\n%!";
  measure None
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:443/nginx/%s"
        c d file)
    values

let _ = csv := add_column !csv "apache ssl" string_of_float data

let _ = Csv.save "timings/bench.csv" !csv

(* ======================== TEST of .chaml ====================== *)

let files = ["bar.html", 50_000]
let values = List.(flatten (map (fun file -> map (fun n -> (file,n)) nb_conn) files))
let _ = csv := []

let _ = csv := add_column !csv "file" fst (List.map fst values)
let _ = csv := add_column !csv "nbc" string_of_int (List.map snd values)

let data =
  Printf.printf "simple_httpd chaml\n%!";
  measure
    (Some "../_build/default/examples/echo.exe --port 8080 -c 2100 -j 8 --log-requests 0 --log-exceptions 0")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d http://localhost:8080/vfs/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd chaml" string_of_float data

let data =
  Printf.printf "simple_httpd chaml ssl\n%!";
  measure
    (Some "../_build/default/examples/echo.exe --port 8443 -c 2100 -j 8 --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -c 2100 -j 8 --port=8443 --log-requests 0 --log-exceptions 0")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:8443/vfs/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd chaml ssl" string_of_float data
(*
let data =
  Printf.printf "simple_https chaml ssl+ktls\n%!";
  measure
    (Some "../_build/default/examples/echo.exe -c 2100 -j 8 --ktls --ssl ../_build/default/tests/domain.crt ../_build/default/tests/domain.key -c 2100 -j 8 --port=8444 --log-requests 0 --log-exceptions 0")
    (fun ((file, d), c) ->
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:8444/vfs/%s"
        c d file)
    values

let _ = csv := add_column !csv "simple_httpd chaml ssl+ktls" string_of_float data
 *)
let data =
  Printf.printf "apache php\n%!";
  measure None
    (fun ((file, d), c) ->
      let file = Filename.remove_extension file ^ ".php" in
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d http://localhost:80/nginx/%s"
        c d file)
    values

let _ = csv := add_column !csv "apache php" string_of_float data

let data =
  Printf.printf "apache php ssl\n%!";
  measure None
    (fun ((file, d), c) ->
      let file = Filename.remove_extension file ^ ".php" in
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:443/nginx/%s"
        c d file)
    values

let _ = csv := add_column !csv "apache php ssl" string_of_float data

let data =
  Printf.printf "nginx php\n%!";
  measure None
    (fun ((file, d), c) ->
      let file = Filename.remove_extension file ^ ".php" in
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d http://localhost:7080/%s"
        c d file)
    values

let _ = csv := add_column !csv "nginx php" string_of_float data

let data =
  Printf.printf "nginx php ssl\n%!";
  measure None
    (fun ((file, d), c) ->
      let file = Filename.remove_extension file ^ ".php" in
      Printf.sprintf "h2load --h1 -m 8 -c %d -n %d https://localhost:7443/%s"
        c d file)
    values

let _ = csv := add_column !csv "nginx php ssl" string_of_float data

let _ = Csv.save "timings/bench_chaml.csv" !csv

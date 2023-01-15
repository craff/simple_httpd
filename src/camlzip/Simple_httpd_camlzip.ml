
module U = Simple_httpd_util
module S = Simple_httpd_server
module BS = Simple_httpd_stream

(* zlib string compression *)
let deflate_string ?(buf_size=16 * 4096) str =
  let zlib_str = Zlib.deflate_init 4 false in
  let in_pos = ref 0 in
  let in_len = String.length str in
  let buf_out = Bytes.make buf_size ' ' in
  let stop = ref false in
  let res = Buffer.create (min buf_size (in_len / 4)) in
  while not !stop do
    let do_flush = !in_pos < in_len in
    let flush = if do_flush then Zlib.Z_NO_FLUSH else Zlib.Z_FINISH in
    let (finished, used_in, used_out) =
      Zlib.deflate_string zlib_str
        str !in_pos (in_len - !in_pos) buf_out 0 buf_size flush
    in
    stop:= finished || (used_out = 0 && !in_pos >= in_len);
    in_pos := !in_pos + used_in;
    Buffer.add_subbytes res buf_out 0 used_out
  done;
  Zlib.deflate_end zlib_str;
  Buffer.contents res

let decode_deflate_stream_ ~buf_size (is:S.byte_stream) : S.byte_stream =
  S.debug ~lvl:4 (fun k->k "wrap stream with deflate.decode");
  let zlib_str = Zlib.inflate_init false in
  let is_done = ref false in
  BS.make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ ->
        Zlib.inflate_end zlib_str;
        BS.close is
      )
    ~consume:(fun self len ->
        if len > self.len then (
          S.Response.fail_raise ~code:400
            "inflate: error during decompression: invalid consume len %d (max %d)"
            len self.len
        );
        self.off <- self.off + len;
        self.len <- self.len - len;
      )
    ~fill:(fun self ->
        (* refill [buf] if needed *)
        if self.len = 0 && not !is_done then (
          is.fill_buf();
          begin
            try
              let finished, used_in, used_out =
                Zlib.inflate zlib_str
                  self.bs 0 (Bytes.length self.bs)
                  is.bs is.off is.len Zlib.Z_SYNC_FLUSH
              in
              is.consume used_in;
              self.off <- 0;
              self.len <- used_out;
              if finished then is_done := true;
              U.debug ~lvl:6 (fun k->k "decode %d bytes as %d bytes from inflate (finished: %b)"
                           used_in used_out finished);
            with Zlib.Error (e1,e2) ->
              S.Response.fail_raise ~code:400
                "inflate: error during decompression:\n%s %s" e1 e2
          end;
          U.debug ~lvl:6 (fun k->k "inflate: refill %d bytes into internal buf" self.len);
        );
      )
    ()

let encode_deflate_stream_ ~buf_size (is:S.byte_stream) : S.byte_stream =
  U.debug ~lvl:4 (fun k->k "wrap stream with deflate.encode");
  let refill = ref true in
  let zlib_str = Zlib.deflate_init 4 false in
  BS.make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _self ->
        U.debug ~lvl:4 (fun k->k "deflate: close");
        Zlib.deflate_end zlib_str;
        BS.close is
      )
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n
      )
    ~fill:(fun self ->
        let rec loop() =
          U.debug ~lvl:6 (fun k->k "deflate.fill.iter out_off=%d out_len=%d"
                       self.off self.len);
          if self.len > 0 then (
            () (* still the same slice, not consumed entirely by output *)
          ) else if not !refill then (
            () (* empty slice, no refill *)
          ) else (
            (* the output was entirely consumed, we need to do more work *)
            is.BS.fill_buf();
            if is.len > 0 then (
              (* try to decompress from input buffer *)
              let _finished, used_in, used_out =
                Zlib.deflate zlib_str
                  is.bs is.off is.len
                  self.bs 0 (Bytes.length self.bs)
                  Zlib.Z_NO_FLUSH
              in
              self.off <- 0;
              self.len <- used_out;
              is.consume used_in;
              U.debug ~lvl:6
                (fun k->k "encode %d bytes as %d bytes using deflate (finished: %b)"
                    used_in used_out _finished);
              if _finished then (
                U.debug ~lvl:4 (fun k->k "deflate: finished");
                refill := false;
              );
              loop()
            ) else (
              (* [is] is done, finish sending the data in current buffer *)
              let _finished, used_in, used_out =
                Zlib.deflate zlib_str
                  is.bs is.off is.len
                  self.bs 0 (Bytes.length self.bs)
                  Zlib.Z_FULL_FLUSH
              in
              assert (used_in = 0);
              self.off <- 0;
              self.len <- used_out;
              if used_out = 0 then (
                refill := false;
              );
              loop()
            )
          )
        in
        try loop()
        with Zlib.Error (e1,e2) ->
          S.Response.fail_raise ~code:400
            "deflate: error during compression:\n%s %s" e1 e2
      )
    ()

let split_on_char ?(f=fun x->x) c s : string list =
  let rec loop acc i =
    match String.index_from s i c with
    | exception Not_found ->
      let acc =
        if i=String.length s then acc
        else f (String.sub s i (String.length s-i)) :: acc
      in List.rev acc
    | j ->
      let acc = f (String.sub s i (j-i)) :: acc in
      loop acc (j+1)
  in
  loop [] 0

let has_deflate headers =
  match
    S.Headers.get "Accept-Encoding" headers
  with
  | Some s -> List.mem "deflate" @@ split_on_char ~f:String.trim ',' s
  | None -> false

let accept_deflate (req:_ S.Request.t) =
  has_deflate (S.Request.headers req)

let not_deflated (resp: S.Response.t) =
  not (has_deflate (S.Response.headers resp))

let has_deflate s =
  try Scanf.sscanf s "deflate, %s" (fun _ -> true)
  with _ -> false

(* decompress [req]'s body if needed *)
let decompress_req_stream_ ~buf_size (req:BS.t S.Request.t) : _ S.Request.t =
  match S.Request.get_header ~f:String.trim req "Transfer-Encoding" with
  (* TODO
    | Some "gzip" ->
      let req' = S.Request.set_header req "Transfer-Encoding" "chunked" in
      Some (req', decode_gzip_stream_)
  *)
  | Some s when has_deflate s ->
    begin match Scanf.sscanf s "deflate, %s" (fun s -> s) with
      | tr' ->
        let body' = S.Request.body req |> decode_deflate_stream_ ~buf_size in
        req
        |> S.Request.set_header "Transfer-Encoding" tr'
        |> S.Request.set_body body'
      | exception _ -> req
    end
  | _ -> req

let compress_resp_stream_
    ~compress_above
    ~buf_size
    (req:_ S.Request.t) (resp:S.Response.t) : S.Response.t =

  (* headers for compressed stream *)
  let update_headers h =
    h
    |> S.Headers.remove "Content-Length"
    |> S.Headers.set "Content-Encoding" "deflate"
  in

  if accept_deflate req && not_deflated resp then (
    match resp.body with
    | `String s when String.length s > compress_above ->
      (* big string, we compress *)
      U.debug ~lvl:4
        (fun k->k "encode str response with deflate (size %d, threshold %d)"
             (String.length s) compress_above);
      let body =
        encode_deflate_stream_ ~buf_size @@ BS.of_string s
      in
      resp
      |> S.Response.update_headers update_headers
      |> S.Response.set_body (`Stream body)

    | `Stream str ->
      U.debug ~lvl:4 (fun k->k "encode stream response with deflate");
      resp
      |> S.Response.update_headers update_headers
      |> S.Response.set_body (`Stream (encode_deflate_stream_ ~buf_size str))

    | `String _ | `Void -> resp
  ) else resp

let middleware
    ?(compress_above=16 * 1024)
    ?(buf_size=16 * 1_024)
    () : S.Middleware.t =
  let buf_size = max buf_size 1_024 in
  fun h req ~resp ->
    let req = decompress_req_stream_ ~buf_size req in
    h req
      ~resp:(fun response ->
          resp @@ compress_resp_stream_ ~buf_size ~compress_above req response)

let setup
    ?compress_above ?buf_size server =
  let m = middleware ?compress_above ?buf_size () in
  U.debug (fun k->k "setup gzip support");
  S.add_middleware ~stage:`Encoding server m

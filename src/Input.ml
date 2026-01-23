(* copied from Bytes.ml *)
external unsafe_get : bytes -> int -> char = "%bytes_unsafe_get"
let index_rec s lim i c =
  let rec fn i =
    if i >= lim then raise Not_found else
      if unsafe_get s i = c then i else fn (i + 1)
  in fn i
let index_rec2 s lim i c1 v1 c2 v2 =
  let rec fn i =
    if i >= lim then raise Not_found else
      begin
        let cr = unsafe_get s i in
        if cr = c1 then (i, v1)
        else if cr = c2 then (i, v2)
        else fn (i + 1)
      end
  in
  fn i
type three = One | Two | Three
let index_rec3 enc s lim i c1 c2 c3 =
  let rec fn i =
    if i >= lim then raise Not_found else
      begin
        let cr = unsafe_get s i in
        if cr = c1 then (i, One)
        else if cr = c2 then (i, Two)
        else if cr = c3 then (i, Three)
        else
          begin
            if cr = '%' then enc := true;
            fn (i+1)
          end
      end
  in fn i

let spf = Printf.sprintf

type hidden = unit
type t = {
  mutable bs: bytes;
  mutable off : int;
  mutable len : int;
  fill_buf: unit -> unit;
  consume: int -> unit;
  close: unit -> unit;
  _rest: hidden;
}

let[@inline] close self = self.close()

let empty = {
  bs=Bytes.empty;
  off=0;
  len=0;
  fill_buf=ignore;
  consume=ignore;
  close=ignore;
  _rest=();
}

let make ?(bs=Bytes.create @@ 64 * 1024) ?(close=ignore) ~consume ~fill () : t =
  let rec self = {
    bs;
    off=0;
    len=0;
    close=(fun () -> close self);
    fill_buf=(fun () ->
        if self.len = 0 then fill self);
    consume=
      (fun n ->
         consume self n
      );
    _rest=();
  } in
  self

let of_chan ?(buf_size=64 * 1024) ic : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> close_in ic)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
        if self.len <= 0 then (
          self.off <- 0;
          self.len <- input ic self.bs 0 (Bytes.length self.bs);
        )
      )
    ()

let of_fd ?(buf_size=64 * 1024) ic : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> Unix.close ic)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
        if self.len <= 0 then (
          self.off <- 0;
          self.len <- Util.read ic self.bs 0 (Bytes.length self.bs));
        )
    ()

let of_client ?(buf_size=64 * 1024) ic : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> Async.Client.close ic)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
       if self.len <= 0 then (
          self.off <- 0;
          self.len <- Async.(Client.read ic self.bs 0 (Bytes.length self.bs));
        )
      )
    ()

module Io = Async.Io

let of_io ?(buf_size=64 * 1024) (sock:Io.t) : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> Io.close sock)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
        if self.len <= 0 then (
          self.off <- 0;
          self.len <- Io.read sock self.bs 0 (Bytes.length self.bs);
        )
      )
    ()

let rec iter f (self:t) : unit =
  self.fill_buf();
  if self.len>0 then (
    f self.bs self.off self.len;
    self.consume self.len;
    (iter [@tailcall]) f self
  )

let to_chan (oc:out_channel) (self:t) = iter (output oc) self

let of_bytes ?(i=0) ?len (bs:bytes) : t =
  let len =
    match len with
    | Some n ->
      if n > Bytes.length bs - i then invalid_arg "Byte_stream.of_bytes";
      n
    | None -> Bytes.length bs - i
  in
  let self =
    make
      ~bs ~fill:ignore
      ~close:(fun self -> self.len <- 0)
      ~consume:(fun self n ->
          self.off <- n + self.off;
          self.len <- self.len - n
        )
      ()
  in
  self.off <- i;
  self.len <- len;
  self

let of_string s : t =
  of_bytes (Bytes.unsafe_of_string s)

(* mainly for tests *)
let of_strings ?(buf_size=64 * 1024) ls : t =
  let buf_size =
    List.fold_left (fun s str -> max s (String.length str)) buf_size ls
  in
  let ls = ref ls in
  make ~bs:(Bytes.make buf_size ' ')
    ~fill:(fun self ->
    if self.len <= 0 then
      match !ls with
      | [] -> ()
      | str::l ->
         ls := l; self.len <- String.length str; self.off <- 0;
         Bytes.blit_string str 0 self.bs 0 self.len)
    ~close:(fun self -> self.len <- 0)
    ~consume:(fun self n ->
      self.off <- n + self.off;
      self.len <- self.len - n)
    ()

module type Output = sig
  val echo : string -> unit
  val printf : ('a, Format.formatter, unit, unit) format4 -> 'a
end

type 'a cont_state =
  Begin | Cont of 'a | End

let of_output fn : t =
  let module T = struct
      type _ Effect.t += Fill : string -> unit Effect.t
    end
  in
  let output s =
    (* NOTE: performing an effect with the empty string,
       means end of file *)
    if s <> "" then Effect.perform (T.Fill s)
  in
  let module Output = struct
      let echo = output
      let buffer = Buffer.create 4096
      let fmt = Format.formatter_of_buffer buffer
      let printf fstr =
        let cont _ =
          output (Buffer.contents buffer);
          Buffer.clear buffer
        in
        Buffer.clear buffer;
        Format.kfprintf cont fmt fstr
    end
  in

  let ktop : (unit, unit) Effect.Deep.continuation cont_state ref = ref Begin in

  let rec self : t = {
      bs = Bytes.create 0;
      off = 0;
      len = 0;
      fill_buf = fill;
      consume = consume;
      close = ignore;
      _rest = ()
    }

  and consume n =
    self.off <- self.off + n;
    self.len <- self.len - n

  and handler : type c. c Effect.t
                     -> ((c,unit) Effect.Deep.continuation -> unit) option =
    fun e ->
           match e with
           | T.Fill s ->
              Some (fun k ->
                  ktop := Cont k;
                  self.bs <- Bytes.unsafe_of_string s;
                  self.off <- 0;
                  self.len <- String.length s);
           | _ -> None
  and fill () =
    let open Effect.Deep in
    match !ktop with
    | Begin ->
       match_with fn (module Output : Output)
         { retc = (fun () -> ())
         ; exnc = (fun e -> raise e)
         ; effc = handler
         }
    | Cont k ->
       ktop := End;
       continue k ()
    | End ->
       ()
  in

  self


let with_file ?buf_size file f =
  let ic = Unix.(openfile file [O_RDONLY] 0) in
  try
    let x = f (of_fd ?buf_size ic) in
    Unix.close ic;
    x
  with e ->
    Unix.close ic;
    raise e

let rec really_input (self:t) bytes offset len =
  self.fill_buf();
  if self.len <= 0 then raise End_of_file
  else if len <= self.len then
    begin
      Bytes.blit self.bs self.off bytes offset len;
      self.consume len
    end
  else
    begin
      Bytes.blit self.bs self.off bytes offset self.len;
      self.consume self.len;
      really_input self bytes (offset+self.len) len
    end

let read_all ~buf (self:t) : string =
  iter (Buffer.add_subbytes buf) self;
  let r = Buffer.contents buf in
  Buffer.clear buf;
  r

(*$= & ~printer:Q.(Print.string)
  "hello world" (of_strings ~buf_size:16 ["hello"; " "; "world"] |> read_all ~buf:(Buffer.create 16))

 *)

let end_of_input (self:t) =
  (self.len = 0 && (self.fill_buf(); self.len = 0))

let eof = '\255'

let read_char (self:t) : char =
  self.fill_buf();
  if self.len <= 0 then eof else
    begin
      let c = unsafe_get self.bs self.off in
      self.consume 1;
      c
    end

let peek_char (self:t) : char =
  self.fill_buf();
  if self.len <= 0 then eof else
    unsafe_get self.bs self.off

(* put [n] bytes from the input into bytes *)
let read_exactly_bytes ~too_short (self:t) (bytes:bytes) (n:int) : unit =
  assert (Bytes.length bytes >= n);
  let offset = ref 0 in
  while !offset < n do
    self.fill_buf();
    let n_read = min self.len (n - !offset) in
    Bytes.blit self.bs self.off bytes !offset n_read;
    offset := !offset + n_read;
    self.consume n_read;
    if n_read=0 then too_short();
  done

let read_until ~buf ~target (self:t) : unit =
  Buffer.clear buf;
  let len = String.length target in
  let c0 = target.[0] in
  let pos = ref 0 in
  while !pos < len do
    self.fill_buf();
    if self.len = 0 then failwith "Unexpected end of input";
    try
      let i = index_rec self.bs (self.off+self.len) self.off c0 in
      let r = i - self.off + 1 in
      if r > self.len then raise Not_found;
      Buffer.add_subbytes buf self.bs self.off (r - 1);
      self.consume r;
      pos := 1;
      while !pos < len && !pos > 0 do
        self.fill_buf();
        let c = unsafe_get self.bs self.off in
        self.consume 1;
        if c <> target.[!pos] then
          begin
            Buffer.add_substring buf target 0 !pos;
            Buffer.add_char buf c;
            pos := 0;
          end
        else incr pos
      done
    with
    | Not_found ->
       Buffer.add_subbytes buf self.bs self.off self.len;
       self.consume self.len
  done

(*$= & ~printer:Q.(Print.string)
  "hell" (let target = "o wor" and buf = Buffer.create 16 in\
          let _ = of_strings ~buf_size:16 ["hello"; " "; "world"]\
                |> read_until ~buf ~target in\
          Buffer.contents buf)
  "hello hell" (let target = "o wor" and buf = Buffer.create 16 in\
          let _ = of_strings ~buf_size:16 ["hello "; "hello"; " "; "world"]\
                |> read_until ~buf ~target in\
          Buffer.contents buf)
*)

let read_path ~buf (self:t) : (string * string list * (string * string) list) =
  Buffer.clear buf;
  let path_components = ref [] in
  let cont = ref true in
  let cont_query = ref true in
  let start = ref 0 in
  let pos = ref 0 in
  let enc = ref false in
  let get_buf len =
    let s = Buffer.sub buf !start len in
    if !enc then (enc := false; Util.percent_decode s) else s
  in
  while !cont do
    self.fill_buf();
    if self.len = 0 then raise (failwith "Unexpected end of input");
    try
      let (i,nb) = index_rec3 enc self.bs (self.off+self.len) self.off '/' '?' ' ' in
      let r = i - self.off + 1 in
      Buffer.add_subbytes buf self.bs self.off r;
      pos := !pos + r;
      self.consume r;
      let len = !pos - 1 - !start in
      if len > 0 then path_components := get_buf len :: !path_components;
      start := !pos;
      match nb with
      | One -> ()
      | Two -> cont := false;
      | Three -> cont := false; cont_query := false
    with Not_found ->
      Buffer.add_subbytes buf self.bs self.off self.len;
      pos := !pos + self.len;
      self.consume self.len;
  done;
  let query = ref [] in
  let last_key = ref "" in
  while !cont_query do
    self.fill_buf();
    if self.len = 0 then raise (failwith "Unexpected end of input");
    try
      let (i,nb) = index_rec3 enc self.bs (self.off+self.len) self.off '=' '&' ' ' in
      let r = i - self.off + 1 in
      Buffer.add_subbytes buf self.bs self.off r;
      pos := !pos + r;
      self.consume r;
      let len = !pos - 1 - !start in
      (match nb with
       | One ->
          last_key := get_buf len
       | Two when !last_key = "" ->
          query := (get_buf len, "") :: !query;
       | Two ->
          query := (!last_key, get_buf len) :: !query; last_key := ""
       | Three when !last_key = "" ->
          query := (get_buf len, "") :: !query; cont_query := false;
       | Three ->
          query := (!last_key, get_buf len) :: !query; cont_query := false);
      start := !pos;
    with Not_found ->
      Buffer.add_subbytes buf self.bs self.off self.len;
      pos := !pos + self.len;
      self.consume self.len;
  done;
  let path = Buffer.sub buf 0 (!pos - 1) in
  Buffer.clear buf;
  (*Printf.printf "%s " path;
  List.iter (fun s -> Printf.printf "@%s" s) !path_components;
  List.iter (fun (k,v) -> Printf.printf "&%s=%s" k v) !query;
  Printf.printf "\n%!";*)
  (path, List.rev !path_components, !query)

(*$= & ~printer:Q.(fun (x,ls,l2s) -> x ^ String.concat " " ls ^ String.concat " " (List.map (fun (x,y) -> x ^ "=" ^ y) l2s))
  ("hello/world?x=z&u=v", ["hello";"world"], [("u","v");("x","z")])\
  (let buf = Buffer.create 16 in\
   of_strings ~buf_size:16 ["hello"; "/"; "world"; "?x="; "z&u"; "="; "v "]\
                |> read_path ~buf)
  ("hello/nice%20world?x=z&u=v%20j", ["hello";"nice world"], [("u","v j");("x","z")])\
  (let buf = Buffer.create 16 in\
   of_strings ~buf_size:16 ["he"; "ll"; "o"; "/nic"; "e%20"; "w"; "orl"; "d?x"; "=z&u"; "="; "v%20j "]\
                |> read_path ~buf)
 *)

(* read a line into the buffer, after clearing it. *)
let read_line_into (self:t) ~buf : unit =
  Buffer.clear buf;
  let continue = ref true in
  while !continue do
    self.fill_buf();
    if self.len=0 then failwith "Unexpected end of input" else
    try
      let j, nb = index_rec2 self.bs (self.off + self.len) self.off '\r' 2 '\n' 1 in
      Buffer.add_subbytes buf self.bs self.off (j - self.off); (* without \r\n *)
      self.consume (j-self.off+nb); (* remove \n/stop *)
      continue := false;
      if self.len < 0 then
        begin
          (* rare case where buffer stops between \r and \n *)
          let to_eat = - self.len in
          self.off <- self.off + self.len;
          self.len <- 0;
          self.fill_buf();
          if self.len = 0 then raise Not_found;
          self.consume to_eat
        end
    with Not_found ->
      Buffer.add_subbytes buf self.bs self.off self.len;
      self.consume self.len;
  done

let read_line ~buf self : string =
  read_line_into self ~buf;
  let len = Buffer.length buf in
  let start = ref 0 in
  while !start < len && Buffer.nth buf !start <= ' ' do incr start done;
  let end_ = ref (len - 1) in
  while !end_ > !start && Buffer.nth buf !end_ <= ' ' do decr end_ done;
  Buffer.sub buf !start (!end_ - !start + 1)

(* new stream with maximum size [max_size].
   @param close_rec if true, closing this will also close the input stream
   @param too_big called with read size if the max size is reached *)
let limit_size_to ~close_rec ~max_size ~too_big (arg:t) : t =
  let size = ref 0 in
  let continue = ref true in
  make
    ~bs:Bytes.empty
    ~close:(fun _ ->
        if close_rec then arg.close ())
    ~fill:(fun res ->
         if res.len = 0 && !continue then (
           arg.fill_buf();
           res.bs <- arg.bs;
           res.off <- arg.off;
           res.len <- arg.len;
         ) else (
           arg.bs <- Bytes.empty;
           arg.off <- 0;
           arg.len <- 0;
         )
      )
    ~consume:(fun res n ->
        size := !size + n;
        if !size > max_size then (
          continue := false;
          too_big !size
        ) else (
          arg.consume n;
          res.off <- res.off + n;
          res.len <- res.len - n;
        ))
    ()

(* read exactly [size] bytes from the stream *)
let read_exactly ~close_rec ~size ~too_short (arg:t) : t =
  if size=0 then (
    empty
  ) else (
    let size = ref size in
    make ~bs:Bytes.empty
      ~fill:(fun res ->
          (* must not block on [arg] if we're done *)
          if !size = 0 then (
            res.bs <- Bytes.empty;
            res.off <- 0;
            res.len <- 0;
          ) else (
            arg.fill_buf();
            res.bs <- arg.bs;
            res.off <- arg.off;
            let len = min arg.len !size in
            if len = 0 && !size > 0 then (
              too_short !size;
            );
            res.len <- len;
          ))
      ~close:(fun _res ->
          (* close underlying stream if [close_rec] *)
          if close_rec then arg.close();
          size := 0
        )
      ~consume:(fun res n ->
          let n = min n !size in
          size := !size - n;
          arg.consume n;
          res.off <- res.off + n;
          res.len <- res.len - n;
        )
      ()
  )

let read_chunked ~buf ~fail ~trailer (bs:t) : t =
  let first = ref true in
  let read_next_chunk_len () : int =
    if !first then (
      first := false
    ) else (
      let line = read_line ~buf bs in
      if String.trim line <> "" then raise (fail
                                              (Printf.sprintf "expected crlf between chunks got %S" line))
    );
    let line = read_line ~buf bs in
    (* parse chunk length, ignore extensions *)
    let chunk_size = (
      if String.trim line = "" then 0
      else
        try Scanf.sscanf line "%x" (fun n -> n)
        with _ -> raise (fail (spf "cannot read chunk size from line %S" line))
    ) in
    chunk_size
  in
  let refill = ref true in
  let chunk_size = ref 0 in
  make
    ~bs:(Bytes.create (16 * 4_096))
    ~fill:(fun self ->
        (* do we need to refill? *)
        if self.len <= 0 then (
          if !chunk_size = 0 && !refill then (
            chunk_size := read_next_chunk_len();
          );
          self.off <- 0;
          if !chunk_size > 0 then (
            (* read the whole chunk, or [Bytes.length bytes] of it *)
            let to_read = min !chunk_size (Bytes.length self.bs) in
            read_exactly_bytes
              ~too_short:(fun () -> raise (fail "chunk is too short"))
              bs self.bs to_read;
            self.len <- to_read;
            chunk_size := !chunk_size - to_read;
          ) else (
            refill := false; (* stream is finished *)
            ignore (trailer bs) (* read trailer *)
          )
        );
      )
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~close:(fun self ->
        (* close this overlay, do not close underlying stream *)
        self.len <- 0;
        refill:= false;
      )
    ()

exception FailParse of int
let fail_parse : t -> 'a = fun self -> raise (FailParse self.off)

let [@inline] branch_char : (char -> t -> 'a) -> t -> 'a = fun fn self ->
  let c = read_char self in
  fn c self

let [@inline] exact_char : char -> 'a -> t -> 'a = fun c r self ->
  let c' = peek_char self in
  if c <> c' then fail_parse self;
  self.consume 1;
  r

let [@inline] charset : (char -> bool) -> t -> char =
  fun cs self ->
  let c = peek_char self in
  if cs c && c <> eof then
    begin
      self.consume 1;
      c
    end
  else
    fail_parse self



let [@inline] exact_string : string -> 'a -> t -> 'a = fun s r self ->
  for i = 0 to String.length s - 1 do
    let c = read_char self in
    if c <> s.[i] then fail_parse self
  done;
  r

let [@inline] star_fold : type a b.(t -> a) -> (a -> b -> b) -> b -> t -> b =
  fun parse fn acc self ->
  let off = ref self.off in
  let acc : b ref = ref acc in
  try
    while true do
      let r = parse self in
      off := self.off;
      acc := fn r !acc
    done
  with FailParse n as e -> if n != self.off then raise e else !acc

let [@inline] star parse self = star_fold parse (fun _ _ -> ()) () self

let [@inline] plus_fold : (t -> 'a) -> ('a -> 'b -> 'b) -> 'b -> t -> 'b =
  fun parse fn acc self ->
  let r = parse self in
  let acc = fn r acc in
  star_fold parse fn acc self

let [@inline] plus parse self = plus_fold parse (fun _ _ -> ()) () self

let [@inline] blank self = plus (exact_char ' ' ()) self
let [@inline] space self = star (exact_char ' ' ()) self
let [@inline] blanks self =
  star (charset (function ' ' | '\t' | '\r' | '\n' -> true
                          | _ -> false)) self
let [@inline] int self =
  let rec fn first r =
    let c = peek_char self in
    if c < '0' || c > '9' then if first then fail_parse self else r else
      (self.consume 1;
       fn false (r * 10 + Char.code c - Char.code '0'));

  in
  fn true 0

let current self = Bytes.sub_string self.bs 0 (self.off + self.len)

(*$= & ~printer:Q.(Print.string)
  "tototitititutux" (of_output (fun (module O) -> O.echo "tototi"; O.echo "tititutu"; O.echo "x") |> read_all ~buf:(Buffer.create 16))

 *)

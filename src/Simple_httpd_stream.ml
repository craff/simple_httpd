
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

let make ?(bs=Bytes.create @@ 16 * 1024) ?(close=ignore) ~consume ~fill () : t =
  let rec self = {
    bs;
    off=0;
    len=0;
    close=(fun () -> close self);
    fill_buf=(fun () ->
        if self.len = 0 then fill self);
    consume=
      (fun n ->
         assert (n <= self.len);
         consume self n
      );
    _rest=();
  } in
  self

let of_chan_ ?(buf_size=16 * 1024) ~close ic : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> close ic)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
        if self.off >= self.len then (
          self.off <- 0;
          self.len <- input ic self.bs 0 (Bytes.length self.bs);
        )
      )
    ()

let of_chan = of_chan_ ~close:close_in

let of_fd_ ?(buf_size=16 * 1024) ~close ic : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> close ic)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
        if self.off >= self.len then (
          self.off <- 0;
          self.len <- Unix.read ic self.bs 0 (Bytes.length self.bs));
        )
    ()

let of_fd = of_fd_ ~close:Unix.close

let of_client_ ?(buf_size=16 * 1024) ~close ic : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> close ic)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
        if self.off >= self.len then (
          self.off <- 0;
          self.len <- Simple_httpd_domain.(read ic self.bs 0 (Bytes.length self.bs));
        )
      )
    ()

let of_client = of_client_ ~close:(fun c -> Simple_httpd_domain.close c)

module Io = Simple_httpd_domain.Io

let of_client_fd_ ?(buf_size=16 * 1024) ~close (sock:Io.t) : t =
  make
    ~bs:(Bytes.create buf_size)
    ~close:(fun _ -> close sock)
    ~consume:(fun self n ->
        self.off <- self.off + n;
        self.len <- self.len - n)
    ~fill:(fun self ->
        if self.off >= self.len then (
          self.off <- 0;
          self.len <-
            Io.read sock self.bs 0 (Bytes.length self.bs);
        )
      )
    ()

let of_client_fd = of_client_fd_ ~close:(fun c -> Io.close c)

let rec iter f (self:t) : unit =
  self.fill_buf();
  if self.len=0 then (
    self.close();
  ) else (
    f self.bs self.off self.len;
    self.consume self.len;
    (iter [@tailcall]) f self
  )

let to_chan (oc:out_channel) (self:t) =
  iter (fun s i len -> output oc s i len) self

let of_bytes ?(i=0) ?len (bs:bytes) : t =
  (* invariant: !i+!len is constant *)
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
          assert (n>=0 && n<= self.len);
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

let with_file ?buf_size file f =
  let ic = Unix.(openfile file [O_RDONLY] 0) in
  try
    let x = f (of_fd ?buf_size ic) in
    Unix.close ic;
    x
  with e ->
    Unix.close ic;
    raise e

let read_all ~buf (self:t) : string =
  let continue = ref true in
  while !continue do
    self.fill_buf();
    if self.len > 0 then (
      Buffer.add_subbytes buf self.bs self.off self.len;
      self.consume self.len;
    );
    assert (self.len >= 0);
    if self.len = 0 then (
      continue := false
    )
  done;
  let r = Buffer.contents buf in
  Buffer.clear buf;
  r

(* put [n] bytes from the input into bytes *)
let read_exactly_ ~too_short (self:t) (bytes:bytes) (n:int) : unit =
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

(* read a line into the buffer, after clearing it. *)
let read_line_into ?stop (self:t) ~buf : unit =
  Buffer.clear buf;
  let continue = ref true in
  let stop = match stop with None -> (fun c -> c <> '\n')
                           | Some c' -> (fun c -> c <> '\n' && c <> c')
  in
  while !continue do
    self.fill_buf();
    if self.len=0 then (
      continue := false;
      if Buffer.length buf = 0 then raise End_of_file;
    );
    let j = ref self.off in
    while !j < self.off + self.len && stop (Bytes.get self.bs !j) do
      incr j
    done;
    if !j-self.off < self.len then (
      Buffer.add_subbytes buf self.bs self.off (!j - self.off); (* without \n *)
      self.consume (!j-self.off+1); (* remove \n/stop *)
      continue := false
    ) else (
      Buffer.add_subbytes buf self.bs self.off self.len;
      self.consume self.len;
    )
  done

let read_line ~buf self : string =
  read_line_into self ~buf;
  let len = Buffer.length buf in
  let start = ref 0 in
  while !start < len && Buffer.nth buf !start <= ' ' do incr start done;
  let end_ = ref (len - 1) in
  while !end_ > !start && Buffer.nth buf !end_ <= ' ' do decr end_ done;
  Buffer.sub buf !start (!end_ - !start + 1)

let read_until ~buf ch self : string =
  read_line_into ~stop:ch self ~buf;
  Buffer.contents buf

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

let read_chunked ~buf ~fail ~trailer (bs:t) : t=
  let first = ref true in
  let read_next_chunk_len () : int =
    if !first then (
      first := false
    ) else (
      let line = read_line ~buf bs in
      if String.trim line <> "" then raise (fail "expected crlf between chunks";)
    );
    let line = read_line ~buf bs in
    (* parse chunk length, ignore extensions *)
    let chunk_size = (
      if String.trim line = "" then 0
      else
        try Scanf.sscanf line "%x %s@\r" (fun n _ext -> n)
        with _ -> raise (fail (spf "cannot read chunk size from line %S" line))
    ) in
    chunk_size
  in
  let refill = ref true in
  let chunk_size = ref 0 in
  make
    ~bs:(Bytes.create (16 * 4096))
    ~fill:(fun self ->
        (* do we need to refill? *)
        if self.off >= self.len then (
          if !chunk_size = 0 && !refill then (
            chunk_size := read_next_chunk_len();
          );
          self.off <- 0;
          self.len <- 0;
          if !chunk_size > 0 then (
            (* read the whole chunk, or [Bytes.length bytes] of it *)
            let to_read = min !chunk_size (Bytes.length self.bs) in
            read_exactly_
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


module Out_buf = struct
  type t = { fd : Simple_httpd_domain.client; b: Bytes.t
           ; mutable o : int; s : int }

  let create ?(buf_size=16* 4_096) fd =
    {fd; s=buf_size; o = 0; b=Bytes.make buf_size ' '}

  let push oc =
    assert(oc.o = oc.s);
    let w = Simple_httpd_domain.write oc.fd oc.b 0 oc.s in
    if w < oc.s then
      begin
        Bytes.blit oc.b w oc.b 0 (oc.s - w);
        oc.o <- oc.s - w
      end
    else
      oc.o <- 0

  let flush oc =
    let n = ref 0 in
    while !n < oc.o do
      let w = Simple_httpd_domain.write oc.fd oc.b !n (oc.o - !n) in
      n := !n + w
    done;
    oc.o <- 0

  let close oc =
    flush oc; Simple_httpd_domain.close oc.fd

  let add_substring oc str offset len =
    if oc.o + len <= oc.s then
      begin
        Bytes.blit_string str offset oc.b oc.o len;
        oc.o <- oc.o + len
      end
    else
      begin
        if oc.s <= 0 then push oc;
        let start, remain =
          if oc.o > 0 then
            begin
              let nb = oc.s - oc.o in
              Bytes.blit_string str offset oc.b oc.o nb;
              oc.o <- oc.s;
              flush oc;
              offset + nb, len - nb
            end
          else
            offset, len
        in
        let n = ref start in
        let r = ref remain in
        while !r > oc.s do
          let str = Bytes.unsafe_of_string str in
          let w = Simple_httpd_domain.write oc.fd str !n !r in
          n := !n + w;
          r := !r - w;
        done;
        Bytes.blit_string str !n oc.b 0 !r;
        oc.o <- !r
      end

  let add_string oc str = add_substring oc str 0 (String.length str)

  let add_bytes oc str =
    add_string oc (Bytes.unsafe_to_string str)

  let add_subbytes oc str offset len =
    add_substring oc (Bytes.unsafe_to_string str) offset len

  let add_char oc c =
    if oc.o >= oc.s then push oc;
    Bytes.set oc.b oc.o c;
    oc.o <- oc.o + 1

  let add_hexa oc n =
    let b = ref 0 in
    while n lsr !b > 0xf do
      b := !b + 4
    done;
    while !b >= 0 do
      let d = (n lsr !b) land 0xf in
      let c = if d < 10 then Char.chr (d + Char.code '0')
              else Char.chr (d - 10 + Char.code 'a')
      in
      add_char oc c;
      b := !b - 4
    done

  let printf oc format =
    let cont s = add_string oc s in
    Printf.ksprintf cont format

  let free_space oc =
    let r = oc.s - oc.o in
    if r = 0 then (flush oc; oc.s) else r

end

(* print a stream as a series of chunks: no allocation! *)
let output_chunked (oc:Out_buf.t) (self:t) : unit =
  let open Out_buf in
  let continue = ref true in
  while !continue do
    (* next chunk *)
    self.fill_buf();
    let n = self.len in
    add_hexa oc n;
    add_string oc "\r\n";
    add_subbytes oc self.bs self.off n;
    add_string oc "\r\n";
    self.consume n;
    if n = 0 then (
      continue := false;
    );
  done;
  ()

(* print a stream as a string of chunks: no allocation! *)
let output_string_chunked (oc:Out_buf.t) (str:string) : unit =
  let open Out_buf in
  let offset = ref 0 in
  let len = String.length str in
  while !offset < len do
    (* next chunk *)
    let max_chunk = Out_buf.free_space oc - 8 in
    let n = min max_chunk (len - !offset) in
    add_hexa oc n;
    add_string oc "\r\n";
    add_substring oc str !offset n;
    add_string oc "\r\n";
    offset := !offset + n;
  done;
  add_string oc "0\r\n\r\n"

let output_str = Out_buf.add_string
let output_bytes = Out_buf.add_bytes

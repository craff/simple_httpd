
type t = { fd : Async.client; b: Bytes.t
           ; mutable o : int; s : int }

let create ?(buf_size=16* 4_096) fd =
  {fd; s=buf_size; o = 0; b=Bytes.make buf_size ' '}

let sock oc = oc.fd.sock

let push oc =
  assert(oc.o = oc.s);
  let w = Async.write oc.fd oc.b 0 oc.s in
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
    let w = Async.write oc.fd oc.b !n (oc.o - !n) in
    n := !n + w
  done;
  oc.o <- 0

let close oc =
  flush oc; Async.Client.close oc.fd

let add_substring oc str offset len =
  if oc.o + len <= oc.s then
    begin
      Bytes.blit_string str offset oc.b oc.o len;
      oc.o <- oc.o + len
    end
  else
    begin
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
        let w = Async.write oc.fd str !n !r in
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

let add_decimal oc n =
  let b = ref 1 in
  while n / !b >= 10 do
    b := 10 * !b
  done;
  let n = ref n in
  while !b > 0 do
    let d = (!n / !b) mod 10 in
    let c = Char.chr (d + Char.code '0') in
    add_char oc c;
    n := !n mod !b;
    b := !b / 10
  done

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
(*
  let free_space oc =
  let r = oc.s - oc.o in
  if r = 0 then (flush oc; oc.s) else r
 *)

(* print a stream as a series of chunks: no allocation! *)
let output_chunked ?synch (oc:t) (self:Input.t) : unit =
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
      continue := false
    ) else (match synch with
           | None -> ()
           | Some f -> flush oc; f ())
  done;
  (*add_string oc "\r\n";*) (* empty trailer required by RFC *)
  ()

let output_str = add_string
let output_bytes = add_bytes

let sendfile oc n fd =
  let r = ref 0 in
  flush oc;
  while !r < n  do
    r := !r + Async.sendfile oc.fd fd !r (n - !r);
  done;

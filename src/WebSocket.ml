
let ws_write ?(binary=true) (cl:Async.client) msg =
  let len = String.length msg in
  let opcode = 0x80 + (if binary then 0x2 else 0x1) in
  let buf = Bytes.create (len + 8) in
  let pos = ref 0 in
  Bytes.set_uint8 buf !pos opcode;
  pos := !pos + 1;
  if len <= 125 then begin
      Bytes.set_uint8 buf !pos len;
      pos := !pos + 1;
    end
  else if len <= 65536 then begin
      Bytes.set_uint8 buf !pos 126;
      Bytes.set_uint16_be buf (!pos+1) len;
      pos := !pos + 3;
    end
  else begin
      Bytes.set_uint8 buf !pos 127;
      Bytes.set_int64_be buf (!pos+1) (Int64.of_int len);
      pos := !pos + 9;
    end;
  Bytes.blit_string msg 0 buf !pos len;
  let len = !pos + len in
  let cur = ref 0 in
  while !cur < len do
    cur := !cur + cl.write buf !cur len
  done;
  cl.flush ()

exception Closed

let ws_read inp =
  let c0 = Char.code (Input.read_char inp) in
  let _fin = (c0 land 0x80) <>  0x0 in
  let code = c0 land 0xf in
  if code = 0x8 || c0 = 0xff then raise Closed;
  let c1 = Char.code (Input.read_char inp) in
  let mask = (c1 land 0x80) <>  0x0 in
  let len = c1 land 0x7f in
  let len =
    if len < 126 then len
    else if len = 126 then
      let c1 = Char.code (Input.read_char inp) in
      let c0 = Char.code (Input.read_char inp) in
      (c1 lsl 8) lor c0
    else if len = 127 then
      let c7 = Char.code (Input.read_char inp) in
      let c6 = Char.code (Input.read_char inp) in
      let c5 = Char.code (Input.read_char inp) in
      let c4 = Char.code (Input.read_char inp) in
      let c3 = Char.code (Input.read_char inp) in
      let c2 = Char.code (Input.read_char inp) in
      let c1 = Char.code (Input.read_char inp) in
      let c0 = Char.code (Input.read_char inp) in
      (c7 lsl 56) lor (c6 lsl 48) lor (c5 lsl 40) lor (c4 lsl 32)
      lor (c3 lsl 24) lor (c2 lsl 16) lor (c1 lsl 8) lor c0
    else
      assert false
  in
  let mask_bytes =
    if mask then
      let c3 = Char.code (Input.read_char inp) in
      let c2 = Char.code (Input.read_char inp) in
      let c1 = Char.code (Input.read_char inp) in
      let c0 = Char.code (Input.read_char inp) in
      [|c3; c2; c1; c0|]
    else
      [|0;0;0;0|]
  in
  let buf = Bytes.create len in
  let too_short () = raise Closed in
  Input.read_exactly_bytes ~too_short inp buf len;
  if mask then
    for i = 0 to len - 1 do
      Bytes.set buf i (Char.chr (Char.code (Bytes.get buf i) lxor mask_bytes.(i land 0x3)))
    done;
  Bytes.unsafe_to_string buf

exception NotWebSocket
let websocket_accept_key client_key =
  let magic_string = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  let to_hash = client_key ^ magic_string in
  let sha1 = Digestif.SHA1.(to_raw_string (digest_string to_hash)) in
  Base64.encode_string sha1

let start_web_socket f req =
  try
    let client = Request.client req in
    let inp = Input.of_client client in
    let read () = ws_read inp in
    let write str = ws_write client str in
    let c = Request.get_header req Headers.Upgrade in
    if c <> Some "websocket" then raise NotWebSocket;
    let client_key =
      match Request.get_header req Headers.Sec_WebSocket_Key with
      | None -> raise NotWebSocket
      | Some k -> k
    in
    let server_key = websocket_accept_key client_key in
    let headers = Headers.empty in
    let headers = Headers.set Headers.Connection "Upgrade" headers in
    let headers = Headers.set Headers.Upgrade "websocket" headers in
    let headers = Headers.set Headers.Sec_WebSocket_Accept server_key headers in
    let response =
      Response.make_void ~code:Response_code.switching_protocols
        ~headers ()
    in
    let wait = Async.spawn (fun () -> f ~read ~write) in
    (wait, response)
  with
    NotWebSocket -> Response.fail_raise ~code:Response_code.bad_request ""

let shell client io2 pid sfd ~read ~write =
(*  let n = ws_write ~binary:false io "HELLO\n" in
  Log.f (Exc 0) (fun k -> k "%d written" n);*)

(*  let s = "stty -echo\nexport TERM=xterm\n" in
  let _ = Io.write io2 (Bytes.unsafe_of_string s) 0 (String.length s) in*)
  let size = 4096 in
  let cont = ref true in
  let reader () =
    try
      while !cont do
        let s = read () in
        if s = "" then raise Closed;
        match s.[0] with
        | 'D' ->
           let _ = Async.Io.write io2 (Bytes.unsafe_of_string s) 1
                     (String.length s - 1) in
           ()
        |'S' ->
          let cols, rows = Scanf.sscanf s "S%d %d" (fun c r -> c, r) in
          Util.resize_ptty sfd ~rows ~cols ~pid;

        | _ -> ()
      done
    with Closed | Unix.Unix_error _ ->
                   cont := false;
                   Async.Client.immediate_close client
  in
  let writer () =
    let buf = Bytes.create size in
    try
      while !cont do
        let s = Async.Io.read io2 buf 0 size in
        if s = 0 then raise Closed;
        let resp = Bytes.sub_string buf 0 s in
        let _ = write resp in
        Async.Client.reset_timeout client;
        ()
      done
    with Closed | Unix.Unix_error _ ->
                   cont := false;
                   Async.Client.immediate_close client
  in
  let check () =
    try
      while !cont do
        let (resp, _) = Unix.(waitpid [WNOHANG] pid) in
        if resp <> 0 then raise Closed;
        Async.sleep 5.0
      done
    with Closed | Unix.Unix_error _ ->
                   cont := false;
                   Async.Client.immediate_close client
  in
  let _wait1 = Async.spawn reader in
  let _wait2 = Async.spawn writer in
  check ()

let terminal_page ?in_head ?css ?start_header ?end_header
      ?start_contents ?end_contents req headers =
  let css = match css with
    | None -> ""
    | Some s -> {html|<link rel="stylesheet" href={`s`}>|html}
  in
  {chaml|
   <!DOCTYPE html>
   <html>
   <head>
   <meta charset="UTF-8">
   <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/xterm@5.3.0/css/xterm.min.css">
   <script src="https://cdn.jsdelivr.net/npm/xterm@5.3.0/lib/xterm.min.js"></script>
   <script src="https://cdn.jsdelivr.net/npm/xterm-addon-fit/lib/xterm-addon-fit.min.js"></script>
   <ml>match in_head with None -> () | Some f -> f output</ml>
   {`css`}
  </head>
   <body>
   <header>
     <ml>match start_header with None -> () | Some f -> f output</ml>
     <h1>Terminal</h1>
     <ml>match end_header with None -> () | Some f -> f output</ml>
   </header>
   <div class = "contents">
     <ml>match start_contents with None -> () | Some f -> f output</ml>
     <div id="terminal"></div>
     <script>
      const term = new Terminal({
	  theme: {
              background: '#1e1e1e',
              foreground: '#ffffff'
	  },
	  cursorBlink: true,
      });
      const fitAddon = new FitAddon.FitAddon();
      term.loadAddon(fitAddon);
      term.open(document.getElementById('terminal'));
      fitAddon.fit();
      term.write('Login: ');

      let username = '';
      // Listen for user input character by character
      let data_handler = term.onData(data => {
	  if (data === '\r') { // Enter pressed
	      term.write('\r\n');
	      startSession(username);
	  } else if (data === '\u007F') { // Backspace
	      if (username.length > 0) {
		  username = username.slice(0, -1);
		  term.write('\b \b');
	      }
	  } else {
	      username += data;
	      term.write(data);
	  }
      });

      function startSession(user) {
	  const protocol = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
	  const ws_url= protocol + window.location.host + '/shell?user=' + user;
	  const socket = new WebSocket(ws_url);

	  let got_prompt = 0;

	  let last = Promise.resolve();

	  socket.onmessage = (event) => {
	      last = last.then(() =>
		  event.data.text().then(str => {
		      term.write(str.replace(/\r?\n/g, '\r\n'));
		      if (got_prompt < 2) {
			  got_prompt += 1;
		      } else if (got_prompt == 2) {
			  got_prompt = 3;
			  console.log("stty");
			  socket.send(`S${term.cols} ${term.rows}`);
		      }
		  }));
	  };

	  socket.onerror = (err) => {
              term.writeln('\r\n❌ Erreur WebSocket');
	  };

	  socket.onclose = (event) => {
              term.writeln('\r\n🔌 Connexion fermée');
	  };

	  socket.onopen = () => {
              term.writeln('✅ Connecté au WebSocket');
	  }

	  window.addEventListener('resize', () => {
	      fitAddon.fit();
	      socket.send(`S${term.cols} ${term.rows}`);
	  });

	  data_handler.dispose();

	  data_handler = term.onData(data => {
	      console.log("input:", data);
	      socket.send("D" + data);
	  });
      }
    </script>
   <ml>match end_contents with None -> () | Some f -> f output</ml>
   </div>
  </body>
</html>
|chaml} req headers

let terminal_handler ?mail req =
  let user = List.assoc "user" (Request.query req) in
  Log.f (Exc 0) (fun k -> k "starting shell for %s" user);
  let client = Request.client req in
  Async.Client.set_timeout client (60. *. 15.);
  begin
    match mail with
    | Some mail ->
       Process.mail mail "Shell started by %s" user;
    | None -> ()
  end;
  let env = [|"TERM=xterm-256color"|] in
  let (pid, fd, sfd) = Util.ptty_spawn "su"
                         [|"su"; "--login"; user|] (Some env)
                         ~usepath:true
                                 ~resetids:true
  in
  Unix.set_nonblock fd;
  let finalise _ =
    Log.f (Exc 0) (fun k -> k "closing shell for %s" user);
    (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
    (try Unix.close fd with Unix.Unix_error _ -> ());
    (try Unix.close sfd with Unix.Unix_error _ -> ());
    (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
  in
  let io2 = Async.Io.create ~client ~finalise fd in
  snd (start_web_socket (shell client io2 pid sfd) req)

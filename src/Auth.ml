
module Client = Async.Client

module type Login = sig
  type t
  val check : login:string -> password:string -> t option (* None: bad login *)
  val login_url : string
end

module Make(Login:Login) = struct

  type auth_state =
     | Logged of Login.t
     | Logging

  let auth_key = Session.new_key ()

  let login_page : Html.chaml =
    (fun request ->
      let check_pass session =
        try
          if Request.meth request <> Method.POST then raise Not_found;
          let login = List.assoc "login" (Request.query request) in
          let password = List.assoc "password" (Request.query request) in
          try
            match Login.check ~login ~password with
            | None -> raise Not_found
            | Some t ->
               Log.f (Aut 0) (fun k->k "Login successful for %S" login);
               Session.set_session_data session auth_key (Logged t);
               true
          with Not_found ->
            Log.f (Aut 0) (fun k -> k "Login failed for %S" login);
            false
        with Not_found ->
          Log.f (Aut 0) (fun k -> k "Login failed: bad form sent");
          false
      in
      let dest =
        try Util.percent_decode (List.assoc "dest" (Request.query request))
        with Not_found -> "/"
      in
      let redirect () =
        let headers = [Headers.Location, dest] in
        Response.fail_raise ~code:Response_code.temporary_redirect
          ~headers "Your are logged"
      in
      begin
        match Session.get_session request with
        | None -> ()
        | Some session ->
           match Session.get_session_data session auth_key with
           | Logged _  -> redirect ()
           | Logging -> if check_pass session then redirect ()
      end;
      {chaml|<!DOCTYPE html>
           <head>
              <title>login page</title>
              <meta charset="UTF-8">
              <script>
                  function subf() {window.history.replaceState( {} , '', "/");}
              </script>
           </head>
           <body>
             <div style="text-align:center; position:absolute;
                         top:50%; left:50%;
                         transform:translate(-50%,-50%);">
               <form action=<?=Login.login_url ^ "?dest=" ^ dest?>
		     onsubmit="subf();" method="post">
                 <table><tr><th><label for="login">Login name</label></tr>
                        <tr><td><input type="text" name="login" value="" on/></tr>
                        <tr><th><label for="password">Password</label></tr>
                        <tr><td><input type="password" name="password" value="" on/></tr>
                        <tr><td><input type="submit" />
                 </table>
               </form>
             </div>
           </body>
       |chaml} request)

  exception Login

  let logout_page dest request =
    begin
      match Session.get_session request with
      | None -> ()
      | Some session ->
         Session.remove_session_data session auth_key;
    end;
    let headers = [(Headers.Location, dest)] in
    Response.fail_raise ~code:Response_code.temporary_redirect
      ~headers "Logged out"

  let check request =
    let destination =
      let path = Request.path request in
      Util.percent_encode path
    in
    let go_login session =
      Session.set_session_data session auth_key Logging;
      raise Login
    in
    let login_url = Printf.sprintf "%s?dest=%s" Login.login_url destination in
    let redirect_headers = [(Headers.Location, login_url)] in
    try
      let error = (Response_code.temporary_redirect,
                   redirect_headers) in
      let (_, session) as r = Session.check ~error request in
      begin
        match Session.get_session_data session auth_key with
        | Logged _ -> ()
        | Logging -> go_login session
        | exception Not_found -> go_login session
      end;
      r
    with Login ->
      Response.fail_raise ~code:Response_code.temporary_redirect
        ~headers:redirect_headers "login please"

end

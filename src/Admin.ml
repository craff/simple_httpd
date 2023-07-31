
module Client = Async.Client

module type Auth = sig
  val password : Digest.t
  val login_url : string
end

module Make(Auth:Auth) = struct

  type auth_state =
     | Logged
     | Logging

  let auth_key = Session.new_key ()

  let login_page : Html.chaml =
    {chaml|<!DOCTYPE html>
            <?ml let dest =
                   try Util.percent_decode (List.assoc "dest" (Request.query request))
                 with Not_found -> "/"
           ?>
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
               <form action=<?=dest?> onsubmit="subf();" method="post">
                 <table><tr><th><label for="password">Admin password</label></tr>
                        <tr><td><input type="password" name="password" value="" on/>
                        <tr><td><input type="submit" />
                 </table>
               </form>
             </div>
           </body>
     |chaml}

  exception Login of string

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
    let destination () =
      let path = Request.path request in
      Util.percent_encode path
    in
    let go_login session =
      Session.set_session_data session auth_key Logging;
      let dest = destination () in
      raise (Login dest)
    in
    let check_pass session =
      try
        if Request.meth request <> Method.POST then raise Not_found;
        let pass = List.assoc "password" (Request.query request) in
        if Digest.string pass <> Auth.password then raise Not_found;
        Session.set_session_data session auth_key Logged
      with Not_found -> go_login session
    in

    try
      let (_, session) as r = Session.check request in
      begin
        match Session.get_session_data session auth_key with
        | Logged  -> ()
        | Logging -> check_pass session
        | exception Not_found -> go_login session
      end;
      r
    with Login dest ->
      let url = Printf.sprintf "%s?dest=%s" Auth.login_url dest in
      let headers = [(Headers.Location, url)] in
      Response.fail_raise ~code:Response_code.temporary_redirect
        ~headers "login please"

end

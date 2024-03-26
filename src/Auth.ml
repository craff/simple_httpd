
module Client = Async.Client

module type Login = sig
  type t
  val check : login:string -> password:string -> t option (* None: bad login *)
  val login_url : string
  val cookie_policy : Session.cookie_policy
end

module Make(Login:Login) = struct

  let cookie_policy = Login.cookie_policy

  let auth_key = Session.new_key ()

  let login_page : ?destination:string -> ?page:Html.chaml -> Html.chaml =
    (fun ?destination ?page request headers ->
      let destination = match destination with
        | Some d -> d
        | None -> try List.assoc "dest" (Request.query request) with Not_found -> ""
      in
      let redirect_headers = [(Headers.Location, Request.path request)] in
      let error = (Response_code.see_other, redirect_headers) in
      let check_pass () =
        try
          if Request.meth request <> Method.POST then raise Not_found;
          let login = List.assoc "login" (Request.query request) in
          let password = List.assoc "password" (Request.query request) in
          let cookies, session = Session.start_check ~cookie_policy ~error request in
          try
            match Login.check ~login ~password with
            | None -> raise Not_found
            | Some t ->
               Log.f (Aut 0) (fun k->k "Login successful for %S" login);
               Session.set_session_data session auth_key t;
               Some cookies
          with Not_found ->
            Log.f (Aut 0) (fun k -> k "Login failed for %S" login);
            None
        with Not_found ->
          Log.f (Aut 0) (fun k -> k "Login failed: bad form sent");
          None
      in
      let redirect cookies =
        let headers = [Headers.Location, destination] in
        Response.fail_raise ~code:Response_code.see_other ~cookies
          ~headers "You are logged"
      in
      let check () =
        try
          match Session.get_session ~cookie_policy request with
          | None -> raise Not_found
          | Some session ->
             ignore (Session.get_session_data session auth_key);
             redirect (Session.mk_cookies session Login.cookie_policy
                         (Request.cookies request))
        with
        | Not_found ->
           match check_pass () with
           | Some cookies -> redirect cookies
           | None -> ()
      in
      check ();

      match page with
      | Some page -> page request headers
      | None ->
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
               <form action=<?=Login.login_url?>
		     onsubmit="subf();" method="post">
                 <table><tr><th><label for="login">Login name</label></tr>
                        <tr><td><input type="text" name="login" value="" on/></tr>
                        <tr><th><label for="password">Password</label></tr>
                        <tr><td><input type="password" name="password" value="" on/></tr>
                        <tr><td><input type="hidden" name="dest" value=<?=destination?> on/></tr>
                        <tr><td><input type="submit" />
                 </table>
               </form>
             </div>
           </body>
       |chaml} request headers)

  exception Login

  let logout_page ?(destination=Login.login_url) ?page request headers =
    let cookies = Session.delete_session ~cookie_policy request in
    match page with
    | Some page ->
       let (h, _, i) = page request headers in
       (h, cookies, i)
    | None ->
       let headers = [Headers.Location, destination] in
       Response.fail_raise ~code:Response_code.see_other ~cookies
         ~headers "You are logged out"

  let check ?nologin ?(check_data=fun _ -> true) request =
    let (log_code, log_headers, log_msg) =
      match nologin with
      | None ->
         let destination =
           let path = Request.path request in
           Util.percent_encode path
         in
         let login_url = Printf.sprintf "%s?dest=%s" Login.login_url destination in
         let redirect_headers = [(Headers.Location, login_url)] in
         (Response_code.see_other, redirect_headers, "login please")
      | Some r -> r
    in
    try
      let error = (log_code, log_headers) in
      let (_, session) as r = Session.start_check ~cookie_policy ~error request in
      begin
        try let data = Session.get_session_data session auth_key in
            if not (check_data data) then
              let cookies = Session.delete_session ~cookie_policy request in
              Response.fail_raise ~code:log_code
                ~headers:log_headers ~cookies "%s" log_msg
        with Not_found -> raise Login
      end;
      r
    with Login ->
         Response.fail_raise ~code:log_code ~headers:log_headers "%s" log_msg


  let check_handler ?not_logged fn request =
    let login_url = Login.login_url in
    let redirect_headers = [(Headers.Location, login_url)] in
    try
      let error = (Response_code.see_other,
                   redirect_headers) in
      try
        let (cookies, session) = Session.start_check ~create:false ~cookie_policy
                                 ~error request in
        fn request cookies (Session.get_session_data session auth_key)
      with Not_found | Headers.Bad_req _ -> raise Login
    with Login ->
      match not_logged with
      | Some fn -> fn request
      | None ->
         Response.fail_raise ~code:Response_code.see_other
           ~headers:redirect_headers "login please"

  let check_filter ?nologin ?(check_data=fun _ -> true) request =
    let (cookies, _) = check ?nologin ~check_data request in
    let fn resp =
      let headers = Headers.set_cookies cookies (Response.headers resp) in
      Response.set_headers headers resp
    in (request, fn)

end

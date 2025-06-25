
module Client = Async.Client

module type Login = sig
  type t
  val check : login:string -> password:string -> t option (* None: bad login *)
  val login_url : string
  val cookie_policy : Session.cookie_policy
end

module Make(Login:Login) = struct

  let cookie_policy = Login.cookie_policy

  let auth_key = Session.new_key ("Login_" ^ Login.login_url)

  let login_page : ?destination:string -> ?css:string -> ?page:Html.chaml
                   -> Html.chaml =
    (fun ?destination ?css ?page request headers ->
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
          try
            match Login.check ~login ~password with
            | None -> raise Not_found
            | Some t ->
               Log.f (Aut 0) (fun k->k "Login successful for %S" login);
               let cookies, session = Session.start_check ~create:true ~cookie_policy
                                        ~error request
               in
               Session.set_session_data session auth_key t;
               Some cookies
          with Not_found ->
            Log.f (Aut 0) (fun k -> k "Login failed for %S" login);
            None
        with Not_found ->
          Log.f (Aut 0) (fun k -> k "Need login");
          None
      in
      let redirect cookies =
        let headers = [Headers.Location, destination] in
        Response.fail_raise ~code:Response_code.see_other ~cookies
          ~headers "You are logged"
      in
      let check () =
        try
          match Session.check_session_cookie ~cookie_policy request with
          | None -> assert false
          | Some session ->
             redirect (Session.mk_cookies session Login.cookie_policy
                         (Request.cookies request))
        with
        | Session.Bad_session_cookie ->
           match check_pass () with
           | Some cookies -> redirect cookies
           | None ->
              Session.select_cookies ~delete:true cookie_policy (Request.cookies request)

      in
      let cookies = check () in

      let (headers, _, inp) =
        match page with
        | Some page -> page request headers
        | None ->
           let css = match css with
             | None -> ""
             | Some s -> {html|<link rel="stylesheet" href=<?=s?>>|html}
           in
         {chaml|<!DOCTYPE html>
           <head>
              <title>login page</title>
              <meta charset="UTF-8">
              <script>
                  function subf() {window.history.replaceState( {} , '', "/");}
              </script>
              <?=css?>
           </head>
           <body>
             <div style="text-align:center; position:absolute;
                         top:50%; left:50%;
                         transform:translate(-50%,-50%);">
               <form action=<?=Login.login_url?>
		     onsubmit="subf();" method="post">
                 <table><tr><th><label for="login">Login</label></tr>
                        <tr><td><input type="text" name="login" value="" required on/></tr>
                        <tr><th><label for="password">Password</label></tr>
                        <tr><td><input type="password" name="password" required value="" on/></tr>
                        <tr><td><input type="hidden" name="dest" value=<?=destination?> on/></tr>
                        <tr><td><input type="submit" />
                 </table>
               </form>
             </div>
           </body>
          |chaml} request headers
      in (headers, cookies, inp))

  exception Login

  let get_session_data request =
    Log.(f (Req 0)) (fun k -> k "get session in get_session_data");
    match Session.get_session ~cookie_policy request with
    | None -> None
    | Some session -> Session.get_session_data session auth_key

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
        match Session.get_session_data session auth_key with
        | Some data ->
           if not (check_data data) then
             begin
               let cookies = Session.delete_session ~cookie_policy request in
               Response.fail_raise ~code:log_code
                 ~headers:log_headers ~cookies "%s" log_msg;
             end;
        | None -> raise Login
      end;
      r
    with Login ->
      let cookies = Session.select_cookies ~delete:true cookie_policy
                      (Request.cookies request)
      in
      Response.fail_raise ~code:log_code ~cookies ~headers:log_headers "%s" log_msg


  let check_handler ?not_logged fn request =
    let login_url = Login.login_url in
    let redirect_headers = [(Headers.Location, login_url)] in
    try
      let error = (Response_code.see_other,
                   redirect_headers) in
      try
        let (cookies, session) =
          Session.start_check ~cookie_policy ~error request
        in
        fn request cookies (match Session.get_session_data session auth_key with
                            | Some d -> d | None -> raise Not_found)
      with Not_found | Headers.Bad_req _ -> raise Login
    with Login ->
      match not_logged with
      | Some fn -> fn request
      | None ->
         let cookies = Session.select_cookies ~delete:true cookie_policy (Request.cookies request) in
         Response.fail_raise ~code:Response_code.see_other ~cookies
           ~headers:redirect_headers "login please"

  let check_filter ?nologin ?(check_data=fun _ -> true) request =
    let (cookies, _) = check ?nologin ~check_data request in
    let fn resp =
      let headers = Headers.set_cookies cookies (Response.headers resp) in
      let resp = Response.set_headers headers resp in
      resp
    in (request, fn)

end

(* echo.ml: a fairly complete example *)
open Simple_httpd

(** Parse command line options *)

(** Default address, ports, this code supports only the configuration of the
    main address and port via command line arguments. *)
let addr = ref "0.0.0.0"
let port = ref 2080
let ssl_port = ref 8443

(** the store used to store all files for all sites *)
let global_top_dir = ref "."

(** ssl certificate, if we have only one IP and one port, we can only have one
    certificate for all site/hostnames, which is not ideal. If you have
    several IP or port, you may have dedicated and certificates, but you have
    to change this. *)
let ssl_cert = ref ""
let ssl_priv = ref ""

(** Server.args provides a bunch and standard option to control the
    maximum number of connections, logs, etc... *)
let args, parameters = Server.args ()

(** We can provide extra parameters *)
let _ =
  Arg.parse (Arg.align ([
      "--addr", Arg.Set_string addr, " set address";
      "-a", Arg.Set_string addr, " set address";
      "--port", Arg.Set_int port, " set port";
      "--ssl-port", Arg.Set_int ssl_port, " set ssl port";
      "--ssl", Tuple[Set_string ssl_cert; Set_string ssl_priv], " give ssl certificate and private key";
      "-p", Arg.Set_int port, " set port";
      "--dir", Arg.String (fun s -> global_top_dir := s), " set the top dir for file path";
    ] @ args)) (fun _ -> raise (Arg.Bad "")) "echo [option]*"

(** Initialize ssl, if needed *)
let ssl =
  if !ssl_cert <> "" then
    Some Address.{ cert = !ssl_cert; priv = !ssl_priv; protocol = Ssl.TLSv1_3 }
  else None

(** Compose the stat filter with the compression filter
    provided by [Simple_httpd.Camlzip], than will compress output
    when [deflate] is accepted. *)
let filter, get_stats =
  let filter_stat, get_stats = Stats.filter () in
  let filter_zip =
    Camlzip.filter ~compress_above:1024 ~buf_size:(16*1024) () in
  (Filter.compose_cross filter_zip filter_stat, get_stats)

(** We build the list of addresses to listen from *)
let addresses = [Address.make ~addr:!addr ~port:!port ()]
let addresses = match ssl with
  | None -> addresses
  | Some ssl ->
     Address.make ~addr:!addr ~port:!ssl_port ~ssl () :: addresses

(** Simple_httpd offers the possibility to use different module for different
    applications or sites managed by the same server. *)

(** First a module common to all web sites *)
module Common = struct
  let addresses = addresses

  (** Some page are password protected *)
  module Login = struct
    type t = unit

    (** in production, use Digest.from_hex with only the encrypted
        password! *)
    let check ~login ~password =
      if login = "admin" && password = "1234" then
        Some () else None

    let login_url = "/login"

    let cookie_policy = Session.default_cookie_policy
  end

  module Secure = Auth.Make(Login)

  let filter_auth = Filter.compose_cross filter Secure.check_filter

  let in_body = {funml|
                 <div style="float: right;">
                   <button onclick="window.location.href='/logout'">
                     Logout
                   </button>
                 </div>|funml}

  module Init(Init:Host.Init) = struct
    (** a status page accessible as /status *)
    let _ = Init.add_route_handler_chaml ~filter:filter_auth Route.(exact "status" @/ return)
              (Status.html ~in_body Init.server)

    (** Access to the statistics computed by the filter*)
    let _ =
      Init.add_route_handler_chaml ~filter:filter_auth
        Route.(exact "stats" @/ return) (get_stats ~in_body)

    (** Login page for the status and statistics *)
    let _ =
      Init.add_route_handler_chaml ~filter
        Route.(exact "login" @/ return) Secure.login_page

    (** Logout *)
    let _ =
      Init.add_route_handler_chaml ~filter
        Route.(exact "logout" @/ return) Secure.logout_page

    let _ =
      Init.add_route_handler_chaml ~filter Route.return
        {chaml|
         <!DOCTYPE html>
         <h1>Template entry point</h1>
         <ul>
           <?ml let hostname = match Request.get_header request Headers.Host with
              | None -> Response.fail_raise ~code:Response_code.bad_request
                        "No Host field in your request"
              | Some h -> h
              let hostname = List.hd (String.split_on_char ':' hostname)
              let url1 = Printf.sprintf "http://%s:%d/" hostname (!port + 1000)
              let url2 = Printf.sprintf "http://%s:%d/" hostname (!port + 2000)
           ?>
           <li> <a href=<?=url1?>>First site on <code><?=url1?></code></a>
           <li> <a href=<?=url2?>>Second site on <code><?=url2?></code></a>
           <li> <a href="status?nb=15">Status of the server</a> This is a bit slow
                (it reads the log files) and should be password protected on a
                real site.
           <li> <a href="stats">Statistics of the server</a>
         </ul>|chaml}

    (** a folder to receive let's encrypt challenge, with the prefix that
        certbot expects. *)
    let top_dir = Filename.concat !global_top_dir "lets_encrypt"
    let _ = Init.add_dir_path ~filter
        ~config:(Dir.config ~download:true
                   ~dir_behavior:Dir.Index_or_lists ())
        ~prefix:".well-known/acme-challenge" top_dir
  end
end

(** A first size, accessible from site1.org or port + 1000 for local test *)
module Site1 = struct
  let addresses =
    let open Address in
    List.map (fun a -> change_port (a.port + 1000) a) addresses @
    List.map (fun a -> change_hosts ["site1.org"; "www.site1.org"] a) addresses


  module Init(Init:Host.Init) = struct
    (** Add a virtual file system VFS, produced by [vfs_pack] from an actual
        folger. The small file will reside in memory, big files will resize in
        a dedicated top_dir. We append the store name of the site to a global
        store name.  By default [vfs_pack] keep all files in memory.  use
        [--max-size], [--perm] and [--destination] to populate the store with
        big files.  *)
    let top_dir = Filename.concat !global_top_dir "site1_store"
    let _ =
      let vfs = Site1.make ~top_dir () in
      Init.add_vfs ~filter ~config:(Dir.config ~download:true
                   ~dir_behavior:Dir.Index ()) vfs

    (** Add a dynamic folder to be served. The path may have common prefix
        with the above VFS. We also  *)
    let top_dir = Filename.concat !global_top_dir "site1_dyn"
    let _ =
      Init.add_dir_path ~filter ~config:(Dir.config ~download:true
                   ~dir_behavior:Dir.Index ()) top_dir
  end
end

(** A first size, accessible from site2.org or port + 2000 for local test *)
module Site2 = struct
  let addresses =
    let open Address in
    List.map (fun a -> change_port (a.port + 2000) a) addresses @
    List.map (fun a -> change_hosts ["site2.org"; "www.site2.org"] a) addresses

  module Init(Init:Host.Init) = struct
    (** Add a virtual file system VFS, produced by [vfs_pack] *)
    let top_dir = Filename.concat !global_top_dir "site2_store"
    let _ =
      let vfs = Site2.make ~top_dir () in
      Init.add_vfs ~filter ~config:(Dir.config ~download:true
                   ~dir_behavior:Dir.Index ()) vfs

  end
end

(** Start the server *)
let _ = Host.start_server parameters [(module Common);(module Site1);(module Site2)]

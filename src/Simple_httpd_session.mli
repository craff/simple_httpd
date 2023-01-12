(** Managment of sessions using cookies *)

val check : ?init:(unit -> Simple_httpd_domain.session_data) ->
            ?remove:bool ->
            ?error:(int * string) ->
            'a Simple_httpd_server.Request.t ->
              (Simple_httpd_server.finaliser, int * string) result


exception Closed

val start_web_socket : (read:(unit -> string) -> write:(string -> unit) -> 'a)
                       -> string Request.t -> (unit -> ('a, exn) result) * Response.t

val terminal_page : ?in_head:Html.elt -> ?css:string ->
                    ?start_header: Html.elt -> ?end_header: Html.elt ->
                    ?start_contents: Html.elt -> ?end_contents: Html.elt ->
                    Html.chaml


val terminal_handler : ?mail:unit Process.mail -> 'b Request.t -> Response.t


val html : ?log_size:int ->
           ?in_head:Html.elt -> ?css:string ->
           ?start_header: Html.elt -> ?end_header: Html.elt ->
           ?start_contents: Html.elt -> ?end_contents: Html.elt ->
           Server.t -> Html.chaml
(** Returns a detailed server status as html *)

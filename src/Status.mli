
val html : ?log_size:int
           -> ?in_head : Html.elt -> ?in_body : Html.elt
           -> Server.t -> Html.chaml
(** Returns a detailed server status as html *)

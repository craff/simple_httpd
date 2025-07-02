(* provide a filter giving very simple statistics. We can do much better
   but be carefull on how to do it *)

(** This a a filter to acquire statistics.
    [let (filter, get) = Stats.filter ()]
    will give you a [Route.filter] and a function [get] returning the statistics
    as a html page
    ["N requests (average response time:
         Tms = T1ms (read) + T2ms (build) + T3ms (send))"]
 *)
val filter : unit -> 'a Route.Filter.t *
                       (?in_head: Html.elt -> ?css:string ->
                        ?start_header: Html.elt -> ?end_header: Html.elt ->
                        ?start_contents: Html.elt -> ?end_contents: Html.elt ->
                        Html.chaml)

val save_name : string
val save : out_channel -> unit
val restore : string * int -> in_channel -> unit

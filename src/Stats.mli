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
                       (?check:(string Request.t -> Cookies.t * Session.t)
                        -> Html.chaml)


type index = int

let unset_index = -1

type t =
  { addr : string
  ; port : int
  ; ssl  : Ssl.context option
  ; reuse : bool
  ; mutable index : index
  }

let make ?(addr="0.0.0.0") ?(port=8080) ?ssl ?(reuse=true) () =
  { addr ; port ; ssl; reuse; index = unset_index }

let register fn addrs =
  let a = Array.of_list addrs in
  (a, Array.mapi (fun i x -> x.index <- i; fn x) a)

let index addr =
  let res = addr.index in
  if addr.index < 0 then
    invalid_arg "add_route: the server is not listening to that adress";
  res

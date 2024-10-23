
type 'a key0 = ..
type (_, _) eq_res = Eq : ('a, 'a) eq_res | NEq : ('a, 'b) eq_res
type 'a key = { eq  : 'b. 'b key0 -> ('a,'b) eq_res
              ; key : 'a key0
              ; cln : 'a -> bool (* cleanup when all client are disconnected,
                                    if bool is true : keep in the data. *)
              ; del : 'a -> unit (* cleanup when session is deleted*)
              ; sav : out_channel -> 'a -> unit
              ; lod : in_channel -> 'a
              ; idx : string }

type cell = D : 'a key * 'a -> cell
type data = cell list

type key_ref = R : 'a key -> key_ref
let key_directory = Hashtbl.create 128

let get_key : string -> key_ref = Hashtbl.find key_directory
let get_idx k = k.idx

let new_key : type a. (a -> bool) -> (a -> unit) ->
                   (out_channel -> a -> unit) ->
                   (in_channel -> a) ->
                   string ->
                   a key =
  fun cln del sav lod key_name ->
  let module M = struct
      type 'a key0 += K : a key0
      let key = K
      let eq : type b. b key0 -> (a,b) eq_res = function
        | K -> Eq
        | _ -> NEq
      let r = { eq; key; cln; del; sav; lod; idx=key_name }
      let _ =
        if Hashtbl.mem key_directory key_name then
          begin
            Printf.eprintf "FATAL ERROR: data key %s allready exists\n%!" key_name;
            exit 1
          end;
        Hashtbl.add key_directory key_name (R r);
    end
  in
  M.r

let search : type a. a key -> data -> a
  = fun key l ->
  let rec fn : data -> a = function
    | [] -> raise Not_found
    | D(k,x):: l ->
       match key.eq k.key, x with
       | Eq, x -> x
       | NEq, _ -> fn l
  in fn l

let add_replace : type a. a key -> a -> data -> data
  = fun key x l ->
  let rec fn : data -> data -> data = fun acc l ->
    match l with
    | [] -> List.rev_append acc [D(key, x)]
    | D(k,_) as c:: l ->
       match key.eq k.key with
       | Eq -> List.rev_append acc (D(k,x) :: l)
       | _ -> fn (c::acc) l
  in fn [] l

let remove : type a. a key -> data -> data
  = fun key l0 ->
  let rec fn : data -> data -> data = fun acc l ->
    match l with
    | [] -> l0
    | D(k,_) as c:: l ->
       match key.eq k.key with
       | Eq -> List.rev_append acc l
       | _ -> fn (c::acc) l
  in fn [] l0

let cleanup k x = k.cln x

let cleanup_delete l =
  List.iter (function D(k,x) ->
                       let b = cleanup k x in
                       if b then k.del x) l

let cleanup_filter l =
  List.filter (function D(k,x) -> cleanup k x) l

let empty = []

let save ch l =
  output_value ch (List.length l);
  List.iter (function D(key, data) ->
               output_value ch key.idx;
               key.sav ch data) (cleanup_filter l)

let load ch =
  let size = input_value ch in
  let res = ref [] in
  for _ = 1 to size do
    let idx = input_value ch in
    let R k = get_key idx in
    let data = input_value ch in
    res := D(k, data) :: !res
  done;
  !res

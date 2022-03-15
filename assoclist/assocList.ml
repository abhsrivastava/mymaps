type ('k, 'v) t = ('k * 'v) list
let rec insert k v m = match m with 
  | [] -> [(k, v)]
  | (eK, eV) :: tl -> let (nK, nV) = if (eK = k) then (k, v) else (eK, eV) in 
                        (nK, nV) :: insert k v tl
let (empty: ('k * 'v) list) = []

let rec find k m = match m with 
  | [] -> None
  | (ek, ev) :: t -> if (ek = k) then Some ev else find k t

let rec remove k m = match m with 
  | [] -> []
  | (ek, ev) :: tl -> if (ek = k) then tl else (ek, ev) :: remove k tl

let rec from_list = function 
  | [] -> empty
  | (ek, ev) :: tl -> from_list tl |> insert ek ev

(** [keys m] is a list of keys in [m], without any duplicates *)
let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare

(** [binding m k] is [(k, v)], where [v] is the value that [k] binds in [m]. 
Requires: [k] is a key in [m] *)
let binding m k = (k, List.assoc k m)

let bindings m = List.map (binding m) (keys m)

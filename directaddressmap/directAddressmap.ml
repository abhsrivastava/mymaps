(** AF: [[| Some 0; Some 1; Some 2; ... |]] represents 
        {): v0, 1: v1, 2: v2, ...} If element [i] of 
        the array is instead [None], then [i] is not 
        bound in the map
    RI: None.
        *)
type 'v t = 'v option array

(** Efficiency: O(c) *)
let create c = Array.make c None

(** Efficiency: O(1) *)
let insert k v m = 
  m.(k) <- Some v

(** Efficiency: O(1) *)
let find k m = 
  m.(k)

(** Efficiency: O(1) *)
let remove k m = 
  m.(k) <- None

(** Efficiency: O(c) *)
let from_list c lst = 
  let a = create c in 
  List.iter (fun (k, v) -> insert k v a) lst;
  a

  (** Efficiency O(c)  *)
let bindings m = 
  let b = ref [] in 
  for k = 0 to Array.length m - 1 do 
    match m.(k) with
    | None -> ()
    | Some v -> b := (k, v) :: !b   
  done;
  !b
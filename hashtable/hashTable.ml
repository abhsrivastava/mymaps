type ('k, 'v) t = {
  hash: 'k -> int;
  mutable size: int; (** number of bindings in the map *)
  mutable buckets: ('k * 'v) list array
}

(** Efficiency: O(c) *)
let create h c = {
  hash = h;
  size = 0;
  buckets = Array.make c []
}

(** [capacity tab] is the number of buckets in [tab] 
    Efficiency: O(1) *)
let capacity tab = Array.length tab.buckets

(** [index k tab] is the index at which the key [k] should be stored 
    in the buckets of [tab] 
    Efficiency: O(1) *)
let index k tab = 
  tab.hash k mod (capacity tab)
  
(** [insert_no_reisze k v tab] inserts a bindings from [k] to [v]
    in [tab] and does not resize the table, regardless 
    of what happens to the load factor. 
    Efficiency: O(L) *)
let insert_no_resize k v tab = 
  let b = index k tab in (* O(1) *)
  let old_bucket = tab.buckets.(b) in
  let trimmed_bucket = List.remove_assoc k old_bucket in
  tab.buckets.(b) <- (k, v) :: trimmed_bucket; (* Efficiency O(L) *)
  if not (List.mem_assoc k old_bucket) then (* Efficiency O(L) *)
    tab.size <- tab.size + 1;
  ()

(** [load_factor tab] is the load factor of [tab] i.e., 
    the number of bindings in [tab] divided by the numbr of buckets. *)
let load_factor tab = 
  (float_of_int tab.size) /. (float_of_int (capacity tab))

(** [rehash tab new_capacity] replaces the buckets array of [tab]
    with a new array of size [new_capacity], and re-inserts all the bindings of [tab]
    into the new array. the keys are re-hsahed, so the bindings are likely to land in 
    the new buckets.
    Efficiency: O(n) where n is the number of bindings in the table *)
let rehash tab new_capacity = 
  let rehash_binding (k, v) = 
    insert_no_resize k v tab
  in
  let rehash_bucket b = 
    List.iter rehash_binding b
  in
  let old_buckets = tab.buckets in
  tab.buckets <- Array.make new_capacity []; (* O(n) linear time on number of bindings *)
  tab.size <- 0;
  Array.iter rehash_bucket old_buckets (* O(n) expected linear time on number of bindings *)

  (** [resize_if_needed] resizes and rehashes [tab] if the load factor 
    is too big or too small. Load factors are allowed to range from 1/2 to 2. *)
let resize_if_needed tab = 
  let lf = load_factor tab in 
  if (lf > 2.0) then
    rehash tab (capacity tab * 2)
  else if (lf > 0.0 && lf < 0.5) then 
    rehash tab (capacity tab / 2)
  else
    ()

let insert k v tab = 
  insert_no_resize k v tab; 
  resize_if_needed tab

(** [remove_no_resize k tab] removes [k] from [tab]
    and does not trigger a resizem regardless of what happens to the load factor 
    Efficiency: O(n) expected linear time *)
let remove_no_resize k tab = 
  let b = index k tab in 
  let old_bucket = tab.buckets.(b) in 
  let trimmed_bucket = List.remove_assoc k tab.buckets.(b) in 
  tab.buckets.(b) <- trimmed_bucket;
  if (List.mem_assoc k old_bucket) then 
    tab.size <- tab.size - 1;
  ()

let remove k tab =
  remove_no_resize k tab;
  resize_if_needed tab

let find k tab = List.assoc_opt k tab.buckets.(index k tab)

let bindings tab = 
  let b = ref [] in 
  for k = 0 to capacity tab - 1 do 
    b := List.append tab.buckets.(k) !b
  done;
  !b
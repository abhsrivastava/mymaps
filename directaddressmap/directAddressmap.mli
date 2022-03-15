(** [t] is the type of maps which binds values of type ['v] to keys of type int*)
type 'v t

(** [create c] creates a map with capacity c. Keys [0] to [c -1] are inbounds. *)
val create : int -> 'v t

(** [insert k v m] mutates map [m] to bind [k] to [v]. 
    If [k] was already bound in [m], that binding is
    replaced by the binding to [v] in the modified map. 
    Requires: [k] is inbound *)
val insert : int -> 'v -> 'v t -> unit

(** [find k m] is [Some v] if [k] is bound to [v] in [m] and None if not. 
    Requires: [k] is in the bounds of [m]*)
val find : int -> 'v t -> 'v option

(** [remove k m] mutates [m] to remove any bindings of [k].
    If [k] was not bound in [m], then the map is not mutated. 
    Requires: [k] is in bound for [m] *)
val remove : int -> 'v t -> unit

(** [from_list c lst] returns a map containing the same bindings 
    as associated list [lst] and with capacity [c].
    Requires: [lst] does not contain any duplicate keys,
    and every key in [lst] is in bounds for capacity [c]. *)
val from_list : int -> (int * 'v) list -> 'v t

(** [bindings m] takes in a map with bindings of type [k] and [v] 
    and converts it into a list of tuples of the same type. 
    There are no duplicates in the output list *)

val bindings : 'v t -> (int * 'v) list
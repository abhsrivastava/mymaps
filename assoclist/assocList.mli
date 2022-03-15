(** [t] is the type of maps which binds keys of type ['k] to values of type ['v]*)
type ('k, 'v) t

(** [empty] is the empty map. *)
val empty : ('k, 'v) t

(** [insert k v m] is the same map as [m], but with an additional 
    binding from [k] to [v]. If [k] was already bound in [m], that 
    binding is replaced by the binding to [v] in the new map. *)
val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

(** [find k m] is [Some v] if [k] is bound to [v] in [m] and None if not. *)
val find : 'k -> ('k, 'v) t -> 'v option

(** [remove k m] is the same map [m], but without any finding for [k]. If [k]
    does not exist in [m], then m is unchanged. *)
val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

(** [from_list list] takes in a list of tuples of type [k] and [v] and returns a map with the same bindings.
    If the list contains duplicates, then the left most binding is preserved. *)
val from_list : ('k * 'v) list -> ('k, 'v) t

(** [bindings m] takes in a map with bindings of type [k] and [v] 
    and converts it into a list of tuples of the same type. 
    There are no duplicates in the output list *)

val bindings : ('k, 'v) t -> ('k * 'v) list
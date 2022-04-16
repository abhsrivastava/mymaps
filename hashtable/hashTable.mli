(** [('k, 'v) t] is the type of the mutable table-based map that binds 
    keys of type ['k] to values of type ['v]. *)
type ('k, 'v) t

(** [insert k v m] mutates map [m] and binds [k] to value [v]. 
    If ['k] was already bound to ['v] then that binding is over-written. *)
val insert: 'k -> 'v -> ('k, 'v) t -> unit

(** [find k m] returns [Some v] if [m] binds [k] to [v], and [None]
    if [m] does not bind to [k]. *)
val find: 'k -> ('k, 'v) t -> 'v option

(** [remove k m] mutates map [m] and removes the binding of [k].
    If [k] is not bound in [m], then no change is made. *)
val remove : 'k -> ('k, 'v) t -> unit

(** [create hash c] creates a new tabl map with capacity [c]
    that will use the [hash] as a function to convert keys of type ['k] to integers.
    Requires: hash to distribute keys uniformly and requires hash to run in constant time *)
val create : ('k -> int) -> int -> ('k, 'v) t

val bindings : ('k, 'v) t -> ('k * 'v) list
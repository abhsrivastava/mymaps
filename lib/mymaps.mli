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

(** test method which will be deleted later *)
val sayHello : string
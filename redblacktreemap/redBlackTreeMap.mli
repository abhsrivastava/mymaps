(** [('k, 'v) t] is a type of immutable table-based map which binds 
    keys of type ['k] to values of type ['v] *)
type ('k, 'v) t

(** [empty] provides a empty red black tree *)
val empty: ('k, 'v) t 

(** [find k m] returns [some v] if [k] is bound to [v] in the [m]. otherwise none *)
val find: 'k -> ('k, 'v) t -> 'v option

(** [bindings m] takes a map containing keys [k] bound to values [v] and 
    returns a list of tuples of the same type. 
    The list does not contain duplicates. 
    The list is sorted on [k]*)
val bindings: ('k, 'v) t -> ('k * 'v) list

(** [insert k v m] binds a new key [k] to a value [v] and returns a new map [m]. 
    If [k] is already bound in the map [m] then the value is overwritten with [v] *)
val insert: 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

type color = Red | Black
type ('k, 'v) t = Leaf | Node of (color * ('k, 'v) t * ('k * 'v) * ('k, 'v) t)

let empty = Leaf

let rec find k = function 
  | Leaf -> None
  | Node (_, l, (x, y), r) -> 
      if k < x then find k l
      else if k > x then find k r
      else Some y

let rec bindings = function 
  | Leaf -> []
  | Node (_, l, t, r) -> List.append (t :: bindings l) (bindings r)

let balance = function 
  | (Black, Node (Red, Node (Red, a, (xk, xv), b), (yk, yv), c), (zk, zv), d)
  | (Black, a, (xk, xv) ,Node (Red, b, (yk, yv), Node (Red, c, (zk, zv), d)))
  | (Black, Node (Red, a, (xk, xv), Node (Red, b, (yk, yv), c)), (zk, zv), d)
  | (Black, a, (xk, xv), Node (Red, Node (Red, b, (yk, yv), c), (zk, zv), d)) 
    -> Node (Red, Node (Black, a, (xk, xv), b), (yk, yv), Node (Black, c, (zk, zv), d))
  | t -> Node t

let rec insert_aux k v = function 
  | Leaf -> Node (Red, Leaf, (k, v), Leaf)
  | Node (c, l, (x, y), r) as n -> 
      if k < x then balance (c, insert_aux k v l, (x, y), r)
      else if k > v then balance (c, l, (x, y), insert_aux k v r)
      else n

let insert k v m = 
  match insert_aux k v m with 
  | Leaf -> failwith "Not possible"
  | Node (_, l, t, r) -> Node (Black, l, t, r) (* always color the root node black *)
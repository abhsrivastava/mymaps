(* Insertion logic is Chris Okasai's method *)
(* Deletion logic is taken from https://ii.uni.wroc.pl/~lukstafi/pmwiki/uploads/Functional/functional-lecture05-red_black_del.pdf *)

type color = Red | Black | DoubleBlack | NegativeBlack
type ('k, 'v) t = Leaf 
                  | DoubleBlackLeaf
                  | Node of (color * ('k, 'v) t * ('k * 'v) * ('k, 'v) t)

let empty = Leaf

let whiten = function 
  | Black -> Red (* whitening black results in red *)
  | DoubleBlack -> Black (* whitening double black results in black *)
  | Red -> NegativeBlack (* whitening red results in Negative Black *) 
  | NegativeBlack -> failwith "negative black cannot be whitened."

let blacken = function 
  | Black -> DoubleBlack (* blackening black results in double black *)
  | Red -> Black (* blackening red results in black *)
  | NegativeBlack -> Red (* blacking negative black results in red *)
  | DoubleBlack -> failwith "double black cannot be blackened "

let rec find k = function 
  | Leaf 
  | DoubleBlackLeaf -> None
  | Node (_, l, (x, y), r) -> 
      if k < x then find k l
      else if k > x then find k r
      else Some y

let rec bindings = function 
  | Leaf 
  | DoubleBlackLeaf -> []
  | Node (_, l, t, r) -> List.append (t :: bindings l) (bindings r)

let rec balance = function 
  | ((Black | DoubleBlack) as clr, Node (Red, Node (Red, a, (xk, xv), b), (yk, yv), c), (zk, zv), d)
  | ((Black | DoubleBlack) as clr, a, (xk, xv) ,Node (Red, b, (yk, yv), Node (Red, c, (zk, zv), d)))
  | ((Black | DoubleBlack) as clr, Node (Red, a, (xk, xv), Node (Red, b, (yk, yv), c)), (zk, zv), d)
  | ((Black | DoubleBlack) as clr, a, (xk, xv), Node (Red, Node (Red, b, (yk, yv), c), (zk, zv), d)) 
    -> Node (whiten clr, Node (Black, a, (xk, xv), b), (yk, yv), Node (Black, c, (zk, zv), d))
  | (DoubleBlack, Node (NegativeBlack, Node(Black, a, w, b), x, Node(Black, c, y, d)), z, e)
    -> Node (Black, balance (Black, Node(Red, a, w, b), x, c), y, Node (Black, d, z, e))
  | (DoubleBlack, a, x, Node (NegativeBlack, Node(Black, b, y, c), z, Node (Black, d, w, e)))
    -> Node (Black, Node(Black, a, x, b), y, balance (Black, c, z, Node(Red, d, w, e)))
  | t -> Node t

let rec insert_aux k v = function 
  | Leaf 
  | DoubleBlackLeaf -> Node (Red, Leaf, (k, v), Leaf)
  | Node (c, l, (x, y), r) as n -> 
      if k < x then balance (c, insert_aux k v l, (x, y), r)
      else if k > x then balance (c, l, (x, y), insert_aux k v r)
      else n

let insert k v m = 
  match insert_aux k v m with 
  | Leaf 
  | DoubleBlackLeaf -> failwith "Not possible"
  | Node (_, l, t, r) -> Node (Black, l, t, r) (* always color the root node black *)

let rec size = function
  | Leaf 
  | DoubleBlackLeaf -> 0
  | Node (_, l, _, r) -> 1 + (size l) + (size r)

let rec max = function 
  | Leaf 
  | DoubleBlackLeaf -> None
  | Node (_, _, t, Leaf) -> Some t
  | Node (_, _, _, r) -> max r

let bubble = function
  | (c1, Node(c2, a, x, b), y, Node (c3, c, z, d)) when c1 = DoubleBlack || c2 = DoubleBlack
      -> balance (blacken c1, Node(whiten c2, a, x, b), y, Node (whiten c3, c, z, d))
  | t -> Node t

let rec delete = function 
  (* cases when node to be removed has no children*)
  | (Red, Leaf, _, Leaf) -> Leaf (* Simple case. the node being removed is red and has no children. 
                                         removing it doesn't break any invariants, so just remove it.*)
  | (Black, Leaf, _, Leaf) -> DoubleBlackLeaf (* We can easily remove the node because of no children, but its black and removing it changes 
                                                      the black height of the tree. So we put a double black node in its place. *)
  (* cases when node to be removed has 1 child *)
  | (Black, Node(Red, cl, ct, cr), _, Leaf)  
  | (Black, Leaf, _, Node(Red, cl, ct, cr)) 
      -> Node (Black, cl, ct, cr) (* Child replaces the parent and takes on Black color *) 
  (* cases when node to be removed has two children *)
  | (c, l, _, r) -> (* code should come here when both L and R are not leaves *)
      match max l with
      | None -> failwith "not possible as L is not a leaf"
      | Some max -> bubble (c, remove (fst max) l, max, r)   
  and remove k = function 
    | Leaf | DoubleBlackLeaf -> Leaf
    | Node (c, l, (xk, xv), r) ->
      if k > xk then bubble (c, l, (xk, xv), remove k r)
      else if k < xv then bubble (c, remove k l, (xk, xv), r)
      else delete (c, l, (xk, xv), r)

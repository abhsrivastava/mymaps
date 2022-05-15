(* Insertion logic is Chris Okasai's method *)
(* Deletion logic is taken from https://ii.uni.wroc.pl/~lukstafi/pmwiki/uploads/Functional/functional-lecture05-red_black_del.pdf *)

type color = Red | Black | DoubleBlack | NegativeBlack
type ('k, 'v) t = Leaf 
                  | DoubleBlackLeaf
                  | Node of (color * ('k, 'v) t * 'k * 'v * ('k, 'v) t)

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
  | Node (_, l, x, y, r) -> 
      if k < x then find k l
      else if k > x then find k r
      else Some y

let rec bindings = function 
  | Leaf 
  | DoubleBlackLeaf -> []
  | Node (_, l, x, y, r) -> List.append ((x, y) :: bindings l) (bindings r)

let rec balance = function 
  | ((Black | DoubleBlack) as clr, Node (Red, Node (Red, a, xk, xv, b), yk, yv, c), zk, zv, d)
  | ((Black | DoubleBlack) as clr, a, xk, xv, Node (Red, b, yk, yv, Node (Red, c, zk, zv, d)))
  | ((Black | DoubleBlack) as clr, Node (Red, a, xk, xv, Node (Red, b, yk, yv, c)), zk, zv, d)
  | ((Black | DoubleBlack) as clr, a, xk, xv, Node (Red, Node (Red, b, yk, yv, c), zk, zv, d))
    -> Node (whiten clr, Node (Black, a, xk, xv, b), yk, yv, Node (Black, c, zk, zv, d))
  | (DoubleBlack, Node (NegativeBlack, Node(Black, a, wk, wv, b), xk, xv, Node(Black, c, yk, yv, d)), zk, zv, e)
    -> Node (Black, balance (Black, Node(Red, a, wk, wv, b), xk, xv, c), yk, yv, Node (Black, d, zk, zv, e))
  | (DoubleBlack, a, xk, xv, Node (NegativeBlack, Node(Black, b, yk, yv, c), zk, zv, Node (Black, d, wk, wv, e)))
    -> Node (Black, Node(Black, a, xk, xv, b), yk, yv, balance (Black, c, zk, zv, Node(Red, d, wk, wv, e)))
  | t -> Node t

let rec insert_aux k v = function 
  | Leaf 
  | DoubleBlackLeaf -> Node (Red, Leaf, k, v, Leaf)
  | Node (c, l, x, y, r) -> 
      if k < x then balance (c, insert_aux k v l, x, y, r)
      else if k > x then balance (c, l, x, y, insert_aux k v r)
      else Node (c, l, x, v, r) (* if the value exists then its overwritten *)

let insert k v m = 
  match insert_aux k v m with 
  | Leaf 
  | DoubleBlackLeaf -> failwith "Not possible"
  | Node (_, l, x, y, r) -> Node (Black, l, x, y, r) (* always color the root node black *)

let rec size = function
  | Leaf 
  | DoubleBlackLeaf -> 0
  | Node (_, l, _, _, r) -> 1 + (size l) + (size r)

let rec max = function 
  | Leaf 
  | DoubleBlackLeaf -> None
  | Node (_, _, xk, xv, Leaf) -> Some (xk, xv)
  | Node (_, _, _, _, r) -> max r

let bubble = function
  | (c1, Node(c2, a, xk, xv, b), yk, yv, Node (c3, c, zk, zv, d)) when c1 = DoubleBlack || c2 = DoubleBlack
      -> balance (blacken c1, Node(whiten c2, a, xk, xv, b), yk, yv, Node (whiten c3, c, zk, zv, d))
  | t -> Node t

let rec remove_aux k = function 
  | Leaf | DoubleBlackLeaf -> Leaf
  | Node (c, l, xk, xv, r) ->
    if k > xk then bubble (c, l, xk, xv, remove_aux k r)
    else if k < xk then bubble (c, remove_aux k l, xk, xv, r)
    else delete (c, l, xk, xv, r)
  and delete = function 
    (* cases when node to be removed has no children*)
    | (Red, Leaf, _, _, Leaf) -> Leaf (* Simple case. the node being removed is red and has no children. 
                                          removing it doesn't break any invariants, so just remove it.*)
    | (Black, Leaf, _, _, Leaf) -> DoubleBlackLeaf (* We can easily remove the node because of no children, but its black and removing it changes 
                                                        the black height of the tree. So we put a double black node in its place. *)
    (* cases when node to be removed has 1 child *)
    | (Black, Node(Red, l, x, y, r), _, _, Leaf)  
    | (Black, Leaf, _, _, Node(Red, l, x, y, r)) 
        -> Node (Black, l, x, y, r) (* Child replaces the parent and takes on Black color *) 
    (* cases when node to be removed has two children *)
    | (c, l, _, _, r) -> (* code should come here when both L and R are not leaves *)
        match max l with
        | None -> failwith "not possible as L is not a leaf"
        | Some max -> bubble (c, remove_aux (fst max) l, fst max, snd max, r)

let remove k m = 
  match remove_aux k m with 
  | Leaf -> Leaf
  | DoubleBlackLeaf -> Leaf
  | Node (_, l, x, y, r) -> Node(Black, l, x, y, r)
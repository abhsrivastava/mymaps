type ('k, 'v) t = ('k * 'v) list
let rec insert k v m = match m with 
  | [] -> [(k, v)]
  | (eK, eV) :: tl -> let (nK, nV) = if (eK = k) then (k, v) else (eK, eV) in 
                        (nK, nV) :: insert k v tl
let (empty: ('k * 'v) list) = []

let rec find k m = match m with 
  | [] -> None
  | (ek, ev) :: t -> if (ek = k) then Some v else find k t

let rec remove k m = match m with 
  | [] -> []
  | (ek, ev) :: tl -> if (ek = k) then tl else (ek, ev) :: remove k tl
  
let sayHello = "Hello World"
type ('k, 'v) t = ('k * 'v) list
let rec insert k v m = match m with 
  | [] -> [(k, v)]
  | (eK, eV) :: tl -> let (nK, nV) = if (eK = k) then (k, v) else (eK, eV) in 
                        (nK, nV) :: insert k v tl
let empty = []

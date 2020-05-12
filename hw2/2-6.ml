type btree = Empty
  | Node of int * btree * btree

let rec max_height = function
  | Empty -> 0
  | Node(_,l,r) -> 1 + max (max_height l) (max_height r) 

let rec min_height = function
  | Empty -> 0
  | Node(_,l,r) -> 1 + min (min_height l) (min_height r) 

let check btree = 
  if btree = Empty then true
  else if max_height btree - min_height btree > 1 then false
  else true 
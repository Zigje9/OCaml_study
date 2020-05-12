type btree = Empty | Node of int * btree * btree 

let rec height = function
    | Empty -> 0
    | Node(_,l,r) -> 1 + max (height l) (height r) 
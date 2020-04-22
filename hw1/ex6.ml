type btree = Empty | Node of int * btree * btree 

let rec notexists num = function
    | Empty -> true
    | Node(i,l,r) -> 
        if num == i then false
        else notexists num l && notexists num r 
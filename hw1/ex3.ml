let rec partition = fun f l ->
    match l with
    | [] -> ([],[])
    | h::t -> let (l1,l2) = partition f t in (if f h then (h::l1, l2) else (l1, h::l2))
  
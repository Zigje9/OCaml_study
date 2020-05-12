let rec rev_append l1 l2 =
    match l1 with
    | [] -> l2
    | hd :: tl -> (rev_append tl [hd]@l2)

let new_list = [] ;;

let uniq l =
  let rec iter l new_list =
    if (List.length l = 0) then new_list
    else 
      match l with
      | [] -> new_list
      | hd :: tl ->
        if ((List.mem hd new_list) = false) then (iter tl (new_list @ [hd]))
        else iter tl new_list in
        iter l new_list 
  


let rec to_list l = 
  match l with
    | "" -> []
    | l -> 
      let h = (String.get l 0) in
      let long = (String.length l) in
      h :: (to_list (String.sub l 1 (long-1)))
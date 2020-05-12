let change_string st1 = String.sub st1 1 (String.length st1 - 1)

let rec count_string st1 st2 =
  if String.length st2 = 0 then 0
  else if String.length st1 < String.length st2 then 0
  else
    let comp = String.sub st1 0 (String.length st2) in
    if comp=st2 then 1 + (count_string (change_string st1) st2)
    else count_string (change_string st1) st2



 
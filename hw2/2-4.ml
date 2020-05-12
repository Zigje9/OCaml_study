let rec repeat (s:string) (n:int) =
  if n=0 then "" 
  else s ^ repeat s (n-1)
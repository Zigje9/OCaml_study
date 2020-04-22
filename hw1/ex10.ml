let rec cartesian l1 l2 = 
    if l1=[] || l2=[] then []
    else let ha::ta,hb::tb = l1,l2 in (ha,hb)::(cartesian [ha] tb)@(cartesian ta l2);;
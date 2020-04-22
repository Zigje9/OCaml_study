let rec fold3 f num l1 l2 l3 =
    match (l1,l2,l3) with
    | ([],[],[]) -> num
    | (ha::ta,hb::tb,hc::tc) -> fold3 f (f num ha hb hc) ta tb tc 
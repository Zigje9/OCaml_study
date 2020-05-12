(* HW 2 *)

(* Prob. 1 *)
type exp = X | INT of int
    | REAL of float
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp
    | INTEGRAL of exp * exp * exp

let rec sigma l u e =
  if l > u then 0.0 else (calc_with_x_value e l) +. (sigma (l +. 1.0) u e)

and integral l u e =
  if l > u then 0.0 else (0.1 *. (calc_with_x_value e l)) +. (integral (l +. 0.1) u e)

and calc_with_x_value e v =
  match e with
  | X -> v
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calc_with_x_value e1 v) +. (calc_with_x_value e2 v)
  | SUB (e1, e2) -> (calc_with_x_value e1 v) -. (calc_with_x_value e2 v)
  | MUL (e1, e2) -> (calc_with_x_value e1 v) *. (calc_with_x_value e2 v)
  | DIV (e1, e2) -> (calc_with_x_value e1 v) /. (calc_with_x_value e2 v)
  | SIGMA (e1, e2, e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    sigma l u e3
  | INTEGRAL (e1, e2 ,e3) ->
    let l = calc_with_x_value e1 v in
    let u = calc_with_x_value e2 v in
    integral l (u -. 0.1) e3

let rec calculate e =
  match e with
  | X -> raise (Failure "free variable x!")
  | INT i -> float_of_int i
  | REAL f -> f
  | ADD (e1, e2) -> (calculate e1) +. (calculate e2)
  | SUB (e1, e2) -> (calculate e1) -. (calculate e2)
  | MUL (e1, e2) -> (calculate e1) *. (calculate e2)
  | DIV (e1, e2) -> (calculate e1) /. (calculate e2)
  | SIGMA (e1, e2, e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
		sigma l u e3
  | INTEGRAL (e1, e2 ,e3) ->
    let l = calculate e1 in
    let u = calculate e2 in
    integral l (u -. 0.1) e3
		
(* Prob. 2 *)
type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let rec diff (ae, s) =
  match ae with
  | CONST i -> CONST 0
  | VAR v -> if v = s then CONST 1 else CONST 0
  | POWER (v, i) -> 
		if v = s then TIMES [CONST i; POWER (v, i-1)] else CONST 0
  | TIMES aes ->
		SUM (List.map (fun ae -> 
					let remaining = List.filter (fun ae' -> (Pervasives.compare ae ae') != 0) aes in   
					TIMES (diff (ae, s)::remaining)
				) aes) 
  | SUM aes -> 
		SUM (List.map (fun ae -> diff (ae, s)) aes)

(* Prob. 3 *)
let rec to_list s = 
	if (String.length s) = 0 then [] 
	else (String.get s 0) :: (to_list (String.sub s 1 ((String.length s) - 1)))

(* Prob. 4 *)
let rec repeat s n =
	if n = 0 then "" 
	else s ^ (repeat s (n - 1))  

(* Prob. 5 *)
let rec count_string s sub =
	if (String.length sub) <= 0 || (String.length s) < (String.length sub) then 0 
	else 
		let sub' = String.sub s 0 (String.length sub) in
		let s' = (String.sub s 1 ((String.length s) - 1)) in 
		if (String.compare sub sub') = 0 then 1 + (count_string s' sub)
		else (count_string s' sub)    

(* Prob. 6 *)
type btree = Empty | Node of int * btree * btree

let rec height = 
	function Empty -> 0 
	| Node (_, l, r) -> 1 + (max (height l) (height r)) 

let rec check t = 
	match t with 
	| Empty -> true 
	| Node (_, l, r) -> (abs ((height l) - (height r))) <= 1

(* Prob. 7 *)
let rec add_if_notexist n lst =  
  match lst with
  | [] -> []
  | hd :: tl -> if n = hd then add_if_notexist n tl else hd :: (add_if_notexist n tl)

let rec uniq lst =
  match lst with
  | [] -> []
  | hd :: tl -> hd :: add_if_notexist hd (uniq tl)

(* Prob. 8 *)
let cartesian lis1 lis2 =
  List.fold_left(
    fun a x ->
      List.fold_left(
        fun b y ->
          b @ [(x, y)]
      ) a lis2
  ) [] lis1

type person = string 
type relationships = person * person list
	
let rec transitive rels =
	let old_rels = uniq rels in
	let cartesian_products = cartesian rels rels in 
	let new_rels = 
		uniq (List.fold_left (fun result ((a,b), (c,d)) -> 
				if b = c then (a,d) :: result 
				else result
			) rels cartesian_products) 
	in 
	if (List.length old_rels) = (List.length new_rels) then rels 
	else transitive new_rels    
		
let rec likes rels person =
	let rels = transitive rels in 
	List.length (List.filter (fun (a,b) -> a = person) rels) 

(* Prob. 9 *)
let selflove rels = 
	let rels = transitive rels in
	List.length (uniq (List.filter (fun (a,b) -> a = b) rels))	
		
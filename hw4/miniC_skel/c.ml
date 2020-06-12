(**********************)
(* 프로그래밍 언어론 HW #4 *)
(*   2015038304 박제구  *)
(**********************)
type program = exp
and exp =
	| SKIP
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
	| LE of exp * exp
	| EQ of exp * exp
	| NOT of exp 
  | IF of exp * exp * exp
	| WHILE of exp * exp 
	| LET of var * exp * exp
	| PROC of var list * exp 
	| CALLV of exp * exp list 
	| CALLR of exp * var list
	| ASSIGN of var * exp 
	| RECORD of (var * exp) list 
	| FIELD of exp * var
	| ASSIGNF of exp * var * exp 
  | READ of var
	| PRINT of exp 
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int
  | Bool of bool
	| Unit
  | Procedure of var list * exp * env
	| Record of record
  | Loc of loc
and loc = int 
and env = (var * loc) list
and mem = (loc * value) list
and record = (var * loc) list

(* conversion of value to string *)
let value2str v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "."  
	| Procedure (params,e,env) -> "Procedure "
  | Record record -> "Record "
	| Loc l -> "Loc "^(string_of_int l)

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::(List.filter (fun (l',_) -> l != l') m)
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int (l) ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

let counter = ref 0
let new_location () = counter:=!counter+1; (!counter)

(* conversion of env to string *)
let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
(* conversion of mem to string *)
let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		
		
exception NotImplemented
exception UndefinedSemantics
exception I_can't_implement
(* if the following variable is set true, gc will work (otherwise, gc simply returns a given memory). *)
let remove_garbage = ref false 

let gc: env * mem -> mem
= fun (env, mem) ->
	if (not !remove_garbage) then mem 
	else 
		raise NotImplemented (* TODO *)
		
let rec eval : program -> env -> mem -> (value * mem)
=fun pgm env mem ->  
	match pgm with
	| READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
	| PRINT e ->
		let v, mem' = eval e env mem in
		let _ = print_endline (value2str v) in
		(v, gc(env,mem')) (* Do not modify *) 
	| SKIP -> (Unit, mem)
	| TRUE -> ((Bool true), mem)
	| FALSE -> ((Bool false), mem)
	| CONST n -> ((Int n), mem)
	| VAR x ->  let loc = apply_env env x in 
		let v1 = apply_mem mem loc in ((v1, mem)) 
  | ADD (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 + n2), mem2) 
		| _ -> raise UndefinedSemantics)     
  | SUB (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 - n2), mem2) 
		| _ -> raise UndefinedSemantics)     
  | MUL (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 * n2), mem2) 
		| _ -> raise UndefinedSemantics)     
  | DIV (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in 
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with 
		| Int n1, Int n2 -> (Int (n1 / n2), mem2) 
		| _ -> raise UndefinedSemantics)
	| LE (e1, e2) -> 
		let v1, mem1 = eval e1 env mem in	
		let v2, mem2 = eval e2 env mem1 in 
		(match v1, v2 with
		| Int n1, Int n2 -> (Bool (n1 <= n2), mem2)
		| _ -> raise UndefinedSemantics)
	| EQ (e1, e2) ->
		let v1, mem1 = eval e1 env mem in
		let v2, mem2 = eval e2 env mem1 in
		(match v1, v2 with
		| Int n1, Int n2 -> (Bool (n1 = n2), mem2)
		| Bool n1, Bool n2 -> (Bool (n1 = n2), mem2)
		| (Unit, Unit) -> (Bool true, mem2)
		| _ -> (Bool false, mem2))
	| NOT e ->
		let v1, mem1 = eval e env mem in
		(match v1 with
		| Bool n1 -> (Bool (not n1), mem1)
		| _ -> raise UndefinedSemantics)
	| IF (e1, e2, e3) -> 
		let v1, mem1 = eval e1 env mem in
		(match v1 with 
		| Bool b -> 
			if b then (eval e2 env mem1)  
			else (eval e3 env mem1)
		| _ -> raise UndefinedSemantics) 
	| WHILE (e1, e2) ->
		let v1, mem1 = eval e1 env mem in
		(match v1 with
		|Bool b -> 
			if b then (let v2, mem2 = eval e2 env mem1 in eval (WHILE (e1, e2)) env mem2)
			else (Unit, mem1)
		| _ -> raise UndefinedSemantics)	
	|	LET (x, e1, e2) -> 
		let new_l = new_location () in 
		let v1, mem1 = eval e1 env mem in
		(eval e2 (extend_env (x, new_l) env) (extend_mem (new_l, v1) mem1))		
	| PROC (x, e) -> (Procedure(x, e, env), mem)
	| CALLV (e, el) -> raise I_can't_implement
	| CALLR (e, vl) -> raise I_can't_implement
	| ASSIGN (v, e) -> 
		let v1, mem1 = eval e env mem in (v1, (extend_mem ((apply_env env v), v1) mem1))
	| RECORD rl -> raise I_can't_implement
	| FIELD (e, v) ->
		let v1, mem1 = eval e env mem in
		(match v1 with
		| Record n ->
			(apply_mem mem1 (apply_env n v), mem1)
		| _ -> raise UndefinedSemantics)
	| ASSIGNF (e1, v, e2)  ->
		let v1, mem1 = eval e1 env mem in
		let v2, mem2 = eval e2 env mem1 in 
		(match v1 with
		| Record n -> 
			(v2, extend_mem ((apply_env n v), v2) mem2) 
		| _ -> raise UndefinedSemantics)
	| SEQ (e1, e2) -> let x, mem1 = eval e1 env mem in (eval e2 env mem1)
	| BEGIN e -> (eval e env mem)

let run : program -> bool -> bool -> unit 
= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
		print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))
	
	

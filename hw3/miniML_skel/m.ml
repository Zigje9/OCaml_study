(**********************)
(* 프로그래밍 언어론 HW #3 *)
(*   2015038304 박제구  *)
(**********************)

type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
  | NEWREF of exp 
  | DEREF of exp
  | SETREF of exp * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)

let value2int v =
  match v with
  | Int n -> n
  | Loc l -> l
  | _ -> raise UndefinedSemantics 

let value2bool v = 
  match v with
  | Bool b -> b
  | _ -> raise UndefinedSemantics

let rec eval : exp -> env -> mem -> value * mem
  = fun exp env mem -> 
  match exp with
  | CONST n -> ((Int n), mem)
  | VAR x -> 
    let l = (apply_env env x) in 
    let int_l = value2int l in
    let v = (apply_mem mem int_l) in (v, mem)
  | ADD(e1, e2) -> 
  let ((x1, mem1), (x2, mem2)) = (eval e1 env mem, eval e2 env mem) in (Int ((value2int x1) + (value2int x2)), mem2)
  | MUL(e1, e2) ->   
  let ((x1, mem1), (x2, mem2)) = (eval e1 env mem, eval e2 env mem) in (Int ((value2int x1) * (value2int x2)), mem2)
  | SUB(e1, e2) ->  
  let ((x1, mem1), (x2, mem2)) = (eval e1 env mem, eval e2 env mem) in (Int ((value2int x1) - (value2int x2)), mem2)
  | DIV(e1, e2) ->  
  let ((x1, mem1), (x2, mem2)) = (eval e1 env mem, eval e2 env mem) in (Int ((value2int x1) / (value2int x2)), mem2)
  | ISZERO e -> 
    let (x, mem1) = (eval e env mem) in
    let int_x = value2int x in
    if int_x = 0 then (Bool true, mem1) else (Bool false, mem1)
  | READ ->
    let x = read_int() in (Int x ,mem)
  | IF (e1, e2, e3) ->
    let (x, mem1) = (eval e1 env mem) in
    let x1 = value2bool x in
    if x1 = true then (eval e2 env mem1) else (eval e3 env mem1)
  | LET (x, e1, e2) -> 
    let new_l = new_location () in 
    let (v, mem1) = eval e1 env mem in (eval e2 (extend_env (x, (Loc new_l)) env) (extend_mem (new_l, v) mem1))
  | LETREC (f, x, e1, e2) ->
    let new_l = new_location () in
    let rec_p = RecProcedure (f, x, e1, env) in (eval e2 (extend_env (f, (Loc new_l)) env) (extend_mem (new_l, rec_p) mem))
  | PROC (x, e) ->
    (Procedure (x, e, env), mem)
  | CALL (e1, e2) -> 
    let new_l = new_location () in
    let new_l2 = new_location () in 
    let (v1, mem1) = eval e1 env mem in
    let (v2, mem2) = eval e2 env mem1 in
    (match v1 with
    | Procedure (x, e, env1) ->      
      let (new_env, new_mem) = (extend_env (x, Loc new_l) env1, extend_mem (new_l, v2) mem2) in (eval e new_env new_mem)
    | RecProcedure(f, x, e, env2) -> 
      let (new_env, new_mem) = (extend_env (f, Loc new_l) env2, extend_mem (new_l, v1) mem2) in   
      let (now_env, now_mem) = (extend_env (x, Loc new_l2) new_env, extend_mem (new_l2, v2) new_mem) in (eval e now_env now_mem)
    | _ -> raise UndefinedSemantics
    )
  | NEWREF e ->
    let new_l = new_location () in
    let (x, mem1) = eval e env mem in ((Loc new_l), extend_mem (new_l, x) mem1)
  | DEREF e ->
    let (x, mem1) = eval e env mem in ((apply_mem mem1 (value2int x)), mem1)
  | SETREF (e1, e2) ->
    let (v1, mem1) = eval e1 env mem in
    let (v2, mem2) = eval e2 env mem1 in (v2, (extend_mem ((value2int v1), v2) mem2))
  | SEQ (e1, e2) ->
    let (x, mem1) = eval e1 env mem in (eval e2 env mem1)
  | BEGIN e ->
    (eval e env mem) 

(* driver code *)
let run : program -> value
= fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 

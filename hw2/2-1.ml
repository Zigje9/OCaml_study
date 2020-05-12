exception FreeVariable

type exp = X 
  | INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
  | DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

let rec eval (x:float) (expr:exp) : exp =
	match expr with
	| X -> REAL x
	| INT i -> INT i
	| REAL f -> REAL f
	| ADD (a, b) -> ADD ((eval x a), (eval x b))
	| SUB (a, b) -> SUB ((eval x a), (eval x b))
	| MUL (a, b) -> MUL ((eval x a), (eval x b))
	| DIV (a, b) -> DIV ((eval x a), (eval x b))
	| SIGMA (a, b, c) -> SIGMA ((eval x a), (eval x b), c)
	| INTEGRAL (a, b, c) -> INTEGRAL ((eval x a), (eval x b), c)
	
let rec calculate (expr:exp) : float = 
  match expr with
	| X -> raise FreeVariable
	| INT i -> float_of_int i
	| REAL f ->  f
	| ADD (a, b) -> ((calculate a) +. (calculate b))
	| SUB (a, b) -> ((calculate a) -. (calculate b))
	| MUL (a, b) -> ((calculate a) *. (calculate b))
	| DIV (a, b) -> ((calculate a) /. (calculate b))
	| SIGMA (a, b, c) -> 
    let st = int_of_float (calculate a) in 
    let en = int_of_float (calculate b) in
		if (st > en) then 0.0
		else ((calculate(eval(float_of_int st) c)) +. (calculate(SIGMA(REAL((float_of_int st) +. 1.0), (REAL(float_of_int en)), c))))
	| INTEGRAL (a, b, c) -> 
    let st = (calculate a) in 
    let en = (calculate b) in
		if (en -. st < 0.1) then 0.0
		else ((calculate (eval st c)) *. 0.1) +. (calculate(INTEGRAL(REAL(st +. 0.1), (REAL en), c)))
 



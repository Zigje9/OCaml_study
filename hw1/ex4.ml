type formula = TRUE | FALSE 
    | NOT of formula 
    | ANDALSO of formula * formula
    | ORELSE of formula * formula 
    | IMPLY of formula * formula
    | LESS of expr * expr
and expr = NUM of int 
    | PLUS of expr * expr 
    | MINUS of expr * expr

let rec exprToint x= 
    match x with
    | NUM x -> x
    | PLUS (a, b) -> (exprToint a)+(exprToint b)
    | MINUS (a, b) -> (exprToint a)-(exprToint b)

let rec boolToformula (b:bool): formula =
    match b with
    | true -> TRUE
    | false -> FALSE

let rec eval (f:formula): bool = 
    match f with
    | TRUE -> true
    | FALSE -> false
    | NOT a -> (match a with
            | TRUE -> false
            | FALSE -> true
            | a -> eval (NOT (boolToformula (eval a)))
            )
    | ANDALSO (a, b) -> (match (a, b) with
                    | (TRUE, TRUE) -> true
                    | (TRUE, FALSE) -> false
                    | (FALSE, TRUE) -> false
                    | (FALSE, FALSE) -> false
                    | (a, b) -> eval (ANDALSO (boolToformula (eval a), boolToformula (eval b)))
                    )
    | ORELSE (a, b) -> (match (a, b) with
                    | (TRUE, TRUE) -> true
                    | (TRUE, FALSE) -> true
                    | (FALSE, TRUE) -> true
                    | (FALSE, FALSE) -> false
                    | (a, b) -> eval (ORELSE (boolToformula (eval a), boolToformula (eval b)))
                    )
    | IMPLY (a, b) -> (match (a, b) with
                    | (TRUE, TRUE) -> true
                    | (FALSE, TRUE) -> true
                    | (TRUE, FALSE) -> false
                    | (FALSE, FALSE) -> true
                    | (a, b) -> eval (IMPLY (boolToformula (eval a), boolToformula (eval b)))
                    )
    | LESS (a, b) -> (exprToint a < exprToint b)


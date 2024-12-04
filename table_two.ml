type bool_expr = 
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec evaluate expr a_var a_val b_var b_val = match expr with
    | Var v -> if v = a_var then a_val else if v = b_var then b_val else failwith "Unexpected variable"
    | Not e -> not (evaluate e a_var a_val b_var b_val)
    | And (e1, e2) -> (evaluate e1 a_var a_val b_var b_val) && (evaluate e2 a_var a_val b_var b_val)
    | Or (e1, e2) -> (evaluate e1 a_var a_val b_var b_val) || (evaluate e2 a_var a_val b_var b_val);;

let combinations = [(true, true); (true, false); (false, true); (false, false)];;

let table_two a_var b_var expr =
    let find (a_val, b_val) = (a_val, b_val, evaluate expr a_var a_val b_var b_val) in
    List.map find combinations;;
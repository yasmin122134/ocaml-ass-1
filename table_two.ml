type bool_expr = 
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec evaluate expr a_val b_val = match expr with
    | Var "a" -> a_val
    | Var "b" -> b_val
    | Not e -> not (evaluate e a_val b_val)
    | And (e1, e2) -> (evaluate e1 a_val b_val) && (evaluate e2 a_val b_val)
    | Or (e1, e2) -> (evaluate e1 a_val b_val) || (evaluate e2 a_val b_val)
    | _ -> failwith "Unexpected variable";;

let combinations = [(true, true); (true, false); (false, true); (false, false)];;

let table_two expr =
    let find (a_val, b_val) = (a_val, b_val, evaluate expr a_val b_val) in
    List.map find combinations;;

let print_table results =
    let print_row (a, b, result) =
        Printf.printf "a = %b, b = %b, result = %b\n" a b result
    in
    List.iter print_row results;;
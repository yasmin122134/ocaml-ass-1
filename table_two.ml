type bool_expr = 
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec evaluate expr a_val b_val = match expr with
    | Var v -> if v = "a" then a_val else if v = "b" then b_val else failwith "Unexpected variable"
    | Not e -> not (evaluate e a_val b_val)
    | And (e1, e2) -> (evaluate e1 a_val b_val) && (evaluate e2 a_val b_val)
    | Or (e1, e2) -> (evaluate e1 a_val b_val) || (evaluate e2 a_val b_val);;

let combinations = [(true, true); (true, false); (false, true); (false, false)];;

let table_two expr =
    let find (a_val, b_val) = (a_val, b_val, evaluate expr a_val b_val) in
    List.map find combinations;;

let print_table results =
    let print_row (a, b, result) =
        Printf.printf "a = %b, b = %b, result = %b\n" a b result
    in
    List.iter print_row results;;

let () =
    let expr1 = And (Var "a", Or (Var "b", Not (Var "a"))) in
    let expr2 = Or (Var "a", And (Var "b", Not (Var "a"))) in
    let expr3 = Not (And (Var "a", Var "b")) in
    let expr4 = And (Not (Var "a"), Not (Var "b")) in
    let expr5 = Or (Not (Var "a"), Not (Var "b")) in
    let expr6 = And (Var "a", Var "b") in

    let results1 = table_two expr1 in
    let results2 = table_two expr2 in
    let results3 = table_two expr3 in
    let results4 = table_two expr4 in
    let results5 = table_two expr5 in
    let results6 = table_two expr6 in

    print_endline "Expression 1:";
    print_table results1;
    print_endline "Expression 2:";
    print_table results2;
    print_endline "Expression 3:";
    print_table results3;
    print_endline "Expression 4:";
    print_table results4;
    print_endline "Expression 5:";
    print_table results5;
    print_endline "Expression 6:";
    print_table results6
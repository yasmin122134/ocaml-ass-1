type bool_expr = 
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec evaluate expr var_val = match expr with
    | Var v -> List.assoc v var_val
    | Not e -> not (evaluate e var_val)
    | And (e1, e2) -> (evaluate e1 var_val) && (evaluate e2 var_val)
    | Or (e1, e2) -> (evaluate e1 var_val) || (evaluate e2 var_val);;

let rec var_val_comb vars = match vars with
    | [] -> [[]]
    | v :: rest ->
        let sub_combinations = var_val_comb rest in
        List.concat [
            List.map (fun comb -> (v, true) :: comb) sub_combinations;
            List.map (fun comb -> (v, false) :: comb) sub_combinations
        ];;



let table vars expr =
    let find var_val = (var_val, evaluate expr var_val) in
    List.map find (var_val_comb vars);;


let expr = And (Var "a", Or (Var "b", Not (Var "c")));;
let vars = ["a"; "b"; "c"];;
let truth_table = table vars expr;;
List.iter (fun (assignments, result) ->
    Printf.printf "%s -> %b\n"
        (String.concat ", " (List.map (fun (v, b) -> Printf.sprintf "%s=%b" v b) assignments))
        result
) truth_table;;

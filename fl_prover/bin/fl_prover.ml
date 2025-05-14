(* fl_prover/bin/f1_prover.ml *)

(* bring in your library *)
open Floyd_hoare_solver


(* Output expressions in a string format *)
let rec str_expr e = 
  match e with
  | Var x -> x
  | Const n -> string_of_int n
  | BinOp (op, left, right) ->
    (
      let op_str = match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      in
      "(" ^ str_expr left ^ " " ^ op_str ^ " " ^ str_expr right ^ ")"
    )
  | UnOp (_, expr) -> "~ (" ^ str_expr expr ^ ")"


(* Output bool expressions in a string format *)
let rec str_bool_expr e =
  match e with
  | Compare (op, left, right) ->
    (
      let op_str = match op with
      | Eq -> "==" | Neq -> "!==" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
      in
      "(" ^ str_expr left ^ op_str ^ str_expr right ^ ")"
    )
  | BoolConst true -> "True"
  | BoolConst false -> "False"
  | BoolBin (logical_op, left, right) ->
    (
      let logical_op_str = match logical_op with
      | And -> "&&" | Or -> "||" in 
      "(" ^ str_bool_expr left ^ logical_op_str ^ str_bool_expr right ^ ")"
    )
  | Not exp -> "NOT(" ^ str_bool_expr exp ^ ")"


let assert_expr_equal actual expected msg =
  if actual = expected then
    Printf.printf "PASS: %s\n" msg
  else
    let exp_str = str_expr expected in
    let act_str = str_expr actual in
      Printf.printf "FAIL: %s\nExpected: %s\nActual:   %s\n"
      msg exp_str act_str


let assert_bool_expr_equal actual expected msg =
  if actual = expected then
    Printf.printf "PASS: %s\n" msg
  else
    let str_exp = str_bool_expr expected in
    let str_act = str_bool_expr actual in
      Printf.printf "FAIL: %s\nExpected: %s\nActual:   %s\n"
      msg str_exp str_act


(* TESTS - for substitutions *)
let () =
  assert_expr_equal
    (substitute_expr ("x", Const 42) (Var "x"))
    (Const 42)
    "Test 1 (Var replacement)";

  assert_expr_equal
    (substitute_expr ("x", Const 42) (BinOp (Add, Var "x", Const 1)))
    (BinOp (Add, Const 42, Const 1))
    "Test 2 (BinOp)";

  assert_expr_equal
    (substitute_expr ("y", Const 5) (BinOp (Mul, Var "x", Var "y")))
    (BinOp (Mul, Var "x", Const 5))
    "Test 3 (Nested Var)";

  assert_expr_equal
    (substitute_expr ("x", BinOp (Add, Const 1, Const 2)) (UnOp (Neg, Var "x")))
    (UnOp (Neg, BinOp (Add, Const 1, Const 2)))
    "Test 4 (UnOp)";
    ()


let () =
  assert_bool_expr_equal
    (sub_bool_expr ("x", Const 3) (Compare (Eq, Var "x", Const 3)))
    (Compare (Eq, Const 3, Const 3))
    "Test 5 (Compare)";

  assert_bool_expr_equal
    (sub_bool_expr ("z", Const 0)
       (BoolBin (And, BoolConst true, Not (Compare (Lt, Var "z", Const 5)))))
    (BoolBin (And, BoolConst true, Not (Compare (Lt, Const 0, Const 5))))
    "Test 6 (BoolBin + Not)";
    ()

    
(* fl_prover/bin/f1_prover.ml *)

(* bring in your library *)
open Floyd_hoare_solver




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

let () =
  let pre = Compare (Eq, Var "x", Const 3) in
  let cmd = Assign ("x", BinOp (Add, Var "x", Const 1)) in
  let post = Compare (Eq, BinOp (Add, Var "x", Const 1), Const 4) in
  let result = prove pre cmd post in
  Printf.printf "Test 7 (Hoare Assignment): %s\n" (if result then "PASS" else "FAIL")


let test1 () =
  let pre = Compare (Eq, Var "x", Const 3) in
  let cmd = Assign ("x", BinOp (Add, Var "x", Const 1)) in
  let post = Compare (Eq, BinOp (Add, Var "x", Const 1), Const 4) in
  let result = prove pre cmd post in
  Printf.printf "Test 1 (Assign): %s\n" (if result then "PASS" else "FAIL")


let test2 () =
  let pre = Compare (Eq, Var "x", Const 1) in
  let cmd =
    Seq (
      Assign ("x", BinOp (Add, Var "x", Const 1)),
      Assign ("x", BinOp (Add, Var "x", Const 2))
    )
  in
  let post = Compare (Eq, Var "x", Const 4) in
  let result = prove pre cmd post in
  Printf.printf "Test 2 (Seq): %s\n" (if result then "PASS" else "FAIL")


let test3 () =
  let pre = BoolConst true in
  let cond = Compare (Eq, Var "flag", Const 0) in
  let then_branch = Assign ("x", Const 1) in
  let else_branch = Assign ("x", Const 2) in
  let cmd = If (cond, then_branch, else_branch) in
  let post = BoolBin (Or,
               Compare (Eq, Var "x", Const 1),
               Compare (Eq, Var "x", Const 2)) in
  let result = prove pre cmd post in
  Printf.printf "Test 3 (If): %s\n" (if result then "PASS" else "FAIL")

let test4 () =
  let pre = Compare (Eq, Var "x", Const 0) in
  let cmd = Assign ("x", BinOp (Add, Var "x", Const 2)) in
  let post = Compare (Eq, Var "x", Const 3) in
  let result = prove pre cmd post in
  Printf.printf "Test 4 (Expected FAIL): %s\n" (if result then "FAIL (wrongly passed)" else "PASS (correctly failed)")


let () = 
  test1 ();
  test2 ();
  test3 ();
  test4 ()



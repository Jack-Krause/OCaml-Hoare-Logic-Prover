(* fl_prover/bin/f1_prover.ml *)

(* bring in your library *)
open Floyd_hoare_solver


(* TESTS - for substitutions *)
(* let () =
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
    () *)



let test1 () =
  (* let pre = Compare (Eq, Var "x", Const 2) in
  let cmd = Assign ("x", BinOp (Add, Var "x", Const 1)) in
  let post = Compare (Eq, BinOp (Add, Var "x", Const 1), Const 4) in *)
  let pre = Compare(Eq, BinOp(Add, Var "x", Const 2), Const 4) in
  let cmd = Assign("x", BinOp(Add, Var "x", Const 1)) in
  let post = Compare(Eq, BinOp(Add, Var "x", Const 1), Const 4) in

  let result = prove pre cmd post in
  Printf.printf "Test 1 (Assign): %s\n" (if result then "PASS" else "FAIL")


let test2 () =
  let pre = Compare (Eq, Var "x", Const 5) in
  let cmd =
    Seq (
      Assign ("x", BinOp (Add, Var "x", Const 2)),
      Assign ("x", BinOp (Sub, Var "x", Const 2))
    )
  in
  let post = Compare (Eq, Var "x", Const 5) in
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


let test5 () =
  let pre = BoolBin (And, Compare(Eq, Var "a", Const 9), Compare(Eq, Var "b", Const 13)) in
  let cmd = 
    Seq (
      Assign ("a", BinOp (Add, Var "a", Var "b")),
      Assign ("b", BinOp (Sub, Var "a", Var "b"))
    ) in
  let post = Compare (Eq, Var "b", Const 9) in
  let result = prove pre cmd post in
  Printf.printf "Test 5 (Seq(Assign, Assign)): %s\n" (if result then "PASS" else "FAIL")
    

  (* NEW GENERATED TESTS *)
  (* 6: Skip should succeed iff pre = post *)
let test6_skip_pass () =
  let pre  = Compare (Eq, Var "x", Const 5) in
  let post = Compare (Eq, Var "x", Const 5) in
  let res  = prove pre Skip post in
  Printf.printf "Test 6 (Skip pass): %s\n" (if res then "PASS" else "FAIL")

let test7_skip_fail () =
  let pre  = Compare (Eq, Var "x", Const 5) in
  let post = Compare (Eq, Var "x", Const 6) in
  let res  = prove pre Skip post in
  Printf.printf "Test 7 (Skip fail): %s\n" (if not res then "PASS" else "FAIL (wrongly passed)")

(* 8: Simplification Smoke — x+0 and 0+x both reduce to x *)
let test8_simplify_expr () =
  let open Floyd_hoare_solver in
  let e1 = simplify_expr (BinOp (Add, Var "x", Const 0)) in
  let e2 = simplify_expr (BinOp (Add, Const 0, Var "x")) in
  Printf.printf "Test 8 (x+0 -> x): %s\n"
    (if e1 = Var "x" && e2 = Var "x" then "PASS" else "FAIL")

(* 9: Boolean simplifications — (x+2 == 4) → (x == 2) *)
let test9_simplify_bool () =
  let open Floyd_hoare_solver in
  let raw = Compare (Eq, BinOp (Add, Var "x", Const 2), Const 4) in
  let simp = simplify_bool_expr raw in
  let expect = Compare (Eq, Var "x", Const 2) in
  Printf.printf "Test 9 (x+2==4 -> x==2): %s\n"
    (if simp = expect then "PASS" else "FAIL -- got %s\n" ^ (str_bool_expr simp))

(* 10: A little nested Seq/If: *)
let test10_nested () =
  (* pre: x=1 *)
  let pre = Compare (Eq, Var "x", Const 1) in
  (* cmd:
       if x==1 then
         x:=x+1;
         x:=x*2
       else
         x:=0
    *)
  let cond = Compare (Eq, Var "x", Const 1) in
  let then_branch =
    Seq (
      Assign ("x", BinOp (Add, Var "x", Const 1)),
      Assign ("x", BinOp (Mul, Var "x", Const 2))
    )
  in
  let else_branch = Assign ("x", Const 0) in
  let cmd = If (cond, then_branch, else_branch) in
  (* post: x == 4  (since (1+1)*2 = 4) *)
  let post = Compare (Eq, Var "x", Const 4) in
  let res = prove pre cmd post in
  Printf.printf "Test 10 (nested If+Seq): %s\n" (if res then "PASS" else "FAIL")



(* Call the test cases *)
let () = 
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6_skip_pass ();
  test7_skip_fail ();
  test8_simplify_expr ();
  test9_simplify_bool ();
  test10_nested ()



(* AST definitions *)

(* binary operator definitions *)
type bin_arith =
  | Add
  | Sub
  | Mul
  | Div

(* unary operator definitions *)
type un_arith = Neg
type cmp = Eq | Neq | Lt | Le | Gt | Ge
type bin_bool = And | Or

type expr =
  | Var of string
  | Const of int
  | BinOp of bin_arith * expr * expr
  | UnOp of un_arith  * expr

type bool_expr =
  (* | Compare of cmp * bool_expr * bool_expr *)
  | Compare of cmp * expr * expr
  | BoolConst of bool
  | BoolBin of bin_bool * bool_expr * bool_expr
  | Not of bool_expr

type cmd = 
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of bool_expr * cmd * cmd
  | While of bool_expr * cmd




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



let rec substitute_expr (sub, e1) e2 =
  match e2 with
  | Var x -> 
    (
      if x = sub then e1
      else Var x
    )
  | Const _ -> e2
  | BinOp (op, left, right) -> 
    (BinOp(op, substitute_expr (sub, e1) left, substitute_expr (sub, e1) right))
  | UnOp (op, expr) -> UnOp (op, substitute_expr (sub, e1) expr)


let rec sub_bool_expr (sub, b1) b2 =
  match b2 with
  | Compare (op, left, right) ->
    (
      Compare 
      (
      op, 
      substitute_expr (sub, b1) left, 
      substitute_expr (sub, b1) right
      )
    )
  | BoolConst _ -> b2
  | BoolBin (op, left, right) ->
    (
      BoolBin
      (
        op,
        sub_bool_expr (sub, b1) left,
        sub_bool_expr (sub, b1) right
      )
    )
  | Not e1 -> 
    (
      Not
      (sub_bool_expr (sub, b1) e1)
    )



let infer_precondition (cmd : cmd) (post : bool_expr) : bool_expr = 
  match cmd with
  | Assign (x, e) -> sub_bool_expr (x, e) post
  | _ -> post



let step_str (d, w) pre cmd post =
  let indent = String.make (2 * (d - 1)) ' ' in
Printf.printf "%s%d.%d: {%s} %s {%s}\n"
  indent d w
  (str_bool_expr pre)
  (match cmd with
   | Skip -> "skip"
   | Assign (x, e) -> Printf.sprintf "%s := %s" x (str_expr e)
   | Seq _ -> "seq"
   | If _ -> "if"
   | While _ -> "while")
  (str_bool_expr post)


  let rec simplify_expr e =
    match e with
    | BinOp (Sub, BinOp (Add, l, r1), r2) when r1 = r2 ->
      simplify_expr l
    | BinOp (Add, BinOp (Sub, l, r1), r2) when r1 = r2 ->
      simplify_expr l
    | BinOp (Add, left, right) ->
      let left = simplify_expr left in
        let right = simplify_expr right in
        (
          match left, right with
          | Const a, Const b -> Const (a + b)
          | _, Const 0 -> left
          | BinOp (Add, inner, Const c), Const d ->
            simplify_expr (BinOp (Add, inner, Const (c + d)))
          | _ -> BinOp (Add, left, right)
        )
    | BinOp (Sub, left, right) ->
      let left = simplify_expr left in
        let right = simplify_expr right in
        (
          match left, right with
          | Const a, Const b -> Const (a - b)
          | _, Const 0 -> left
          | _ -> BinOp (Sub, left, right)
        )
    | BinOp (Mul, left, right) ->
      let left = simplify_expr left in
        let right = simplify_expr right in
        (
          match left, right with
          | Const a, Const b -> Const (a * b)
          | _, Const 1 -> left
          | _ -> BinOp (Mul, left, right)
        )
    | BinOp (Div, left, right) ->
      let left = simplify_expr left in
        let right = simplify_expr right in
        (
          match left, right with
          | Const a, Const b -> Const (a / b)
          | _, Const 1 -> left
          | _ -> BinOp (Div, left, right)
        )
    | UnOp (Neg, exp) ->
      (
        match simplify_expr exp with
        | Const a -> Const (-a)
        | exp2 -> UnOp (Neg, exp2)
      )
    | Var _ | Const _ -> e



let rec simplify_bool_expr b =
  let b =
    match b with
    | Compare (Eq, BinOp (Add, left, Const c), Const d) ->
      (
        let diff = d - c in
          simplify_bool_expr (Compare (Eq, left, Const diff))
      )
    | Compare (Eq, Const d, BinOp (Add, left, Const c)) ->
      (
        let diff = d - c in
          simplify_bool_expr (Compare (Eq, left, Const diff))
      )
    | Compare (op, l, r) ->
        let l' = simplify_expr l in
        let r' = simplify_expr r in
        (match l', r' with
         | Const a, Const b ->
             let v = match op with
               | Eq  -> a = b | Neq -> a <> b
               | Lt  -> a < b  | Le  -> a <= b
               | Gt  -> a > b  | Ge  -> a >= b
             in BoolConst v
         | _ ->
             Compare (op, l', r'))
    
    | BoolBin (Or, BoolConst false, x) -> x
    | BoolBin (Or, BoolConst true,  _) -> BoolConst true
    | BoolBin (Or, x, BoolConst false) -> x
    | BoolBin (Or, _, BoolConst true)  -> BoolConst true

    | BoolConst _ as x -> x

    | BoolBin (And, x, y) ->
        BoolBin (And, simplify_bool_expr x, simplify_bool_expr y)

    | BoolBin (Or,  x, y) ->
        BoolBin (Or,  simplify_bool_expr x, simplify_bool_expr y)

    | Not x ->
        Not (simplify_bool_expr x)
  in
  match b with
  | BoolBin (And, BoolConst true,  x) -> x
  | BoolBin (And, BoolConst false, _) -> BoolConst false
  | BoolBin (And, x, BoolConst true)  -> x
  | BoolBin (And, _, BoolConst false) -> BoolConst false

  | BoolBin (Or, Not a, b) when a = b -> BoolConst true
  | BoolBin (Or, a, Not b) when a = b -> BoolConst true

  | BoolBin (Or, BoolConst false, x)  -> x
  | BoolBin (Or, BoolConst true,  _)  -> BoolConst true
  | BoolBin (Or, x, BoolConst false)  -> x
  | BoolBin (Or, _, BoolConst true)   -> BoolConst true

  | Not (BoolConst true)  -> BoolConst false
  | Not (BoolConst false) -> BoolConst true

  | _ -> b


    

let prove (pre : bool_expr) (cmd : cmd) (post : bool_expr) : bool = 
  let rec aux (d, w) pre_c c post_c =
    step_str (d, w) pre_c c post_c;
    match c with
    | Skip ->
      (
          pre_c = post_c
      )
    | Assign (x, e1) ->
      (
        let wp = simplify_bool_expr (sub_bool_expr (x, e1) post_c) in
          let pre_s = simplify_bool_expr pre_c in
            let imp = simplify_bool_expr (BoolBin (Or, Not pre_s, wp)) in
              imp = BoolConst true
      )
    | Seq (c1, c2) ->
      (
        let mid = infer_precondition c2 post_c in
          let r1 = aux (d + 1, 1) pre_c c1 mid in
            let r2 = aux (d + 1, 2) mid c2 post_c in
              r1 && r2
        (* let q = (aux (d + 1, 1) pre_c c1 post_c) in
          (aux (d + 1, 2) q post_c) *)
      )
    | If (b_exp, c1, c2) ->
      (
        let pre_1 = BoolBin (And, pre_c, b_exp) in
          let pre_2 = BoolBin (And, pre_c, Not b_exp) in
            let r1 = aux (d + 1, 1) pre_1 c1 post_c in
              let r2 = aux (d + 1, 2) pre_2 c2 post_c in
                r1 && r2 
        (* if b_exp then aux (d + 1, 1) pre_c c1 post_c
        else aux (d + 1, 1) pre_c c2 post_c  *)
      )
    | While (_, _) ->
      (
        Printf.printf "Skipping while (need inv)\n";
        false
      )


  in aux (1, 1) pre cmd post







  


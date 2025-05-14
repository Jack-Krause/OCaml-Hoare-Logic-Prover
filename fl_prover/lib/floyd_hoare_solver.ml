
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
  

(* evaluate post-conditions after assignments *)
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




  


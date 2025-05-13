
(* AST definitions *)

(* binary operator definitions *)
type bin_arith =
  | Add
  | Sub
  | Mul
  | Div

(* unary operator definitions *)
type un_arith = Neg

type cmp = Eq | New | Lt | Le | Gt | Ge
type bin_bool = And | Or

type bool_expr = 
  | BoolConst of bool
  | Compare of cmp * expr * expr
  | BoolBin of bin_bool * bool_expr * bool_expr



type expr =
  | Var of string
  | Const of int
  | BinOp of 



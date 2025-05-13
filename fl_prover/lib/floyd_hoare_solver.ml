
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

type expr =
  | Var of string
  | Const of int
  | BinOp of bin_arith * expr * expr
  | UnOp of un_arith  * expr

type bool_expr =
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



(* evaluate post-conditions after*)




  


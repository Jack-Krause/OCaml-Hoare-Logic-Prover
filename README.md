# OCaml-Hoare-Logic-Prover

## Floyd-Hoare Logic
"Axiomatic Semantics" - a logical system built from axioms and inference rules.

This system is used for formal proofs about a property (or post-condition) of the state of a program after execution.
The state refers to the value of the program's variables.
The proof assumes a property (or pre-condition) regarding the state before the execution of the program.

The goal of Floyd-Hoare logic is to create statements in the form of ```{P} C {Q}```, where:

P: logical statements about the state before execution (pre-conditions)

Q: logical statements about the state after execution (post-conditions)

C: the program

## Details
```{P} C {Q}``` is a "partial correctness" statement, which doesn't worry about proving that C also will terminate.

Total correctness: ```[P] C [Q]```

## Usage (In Ocaml toplevel)

#### Setup (created) Ocaml Env
(in Powershell)


```
cd fl_prover
& opam env --switch=4.14.0 --shell=powershell | Invoke-Expression
dune clean
dune build
dune exec bin/fl_prover.exe
dune utop
```


Close the Dune utop with ctrl+D


#### Example Usage

(ignore the first '#'s)


```
utop # #require "fl_prover";;
utop # open Floyd_hoare_solver;;

utop # let pre = Compare (Eq, Var "x", Const 0);;
val pre : bool_expr = Compare (Eq, Var "x", Const 0)

utop # let cmd = Assign ("x", BinOp (Add, Var "x", Const 1));;
val cmd : cmd = Assign ("x", BinOp (Add, Var "x", Const 1))

utop # let post = Compare (Eq, Var "x", Const 1);;
val post : bool_expr = Compare (Eq, Var "x", Const 1)

utop # prove pre cmd post;;
1.1: {(x==0)} x := (x + 1) {(x==1)}
```


actual toplevel inputs:


```
#require "fl_prover";;
open Floyd_hoare_solver;;
let pre = Compare (Eq, Var "x", Const 0);;
let cmd = Assign ("x", BinOp (Add, Var "x", Const 1));;
let post = Compare (Eq, Var "x", Const 1);;
prove pre cmd post;;
```


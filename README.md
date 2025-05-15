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

## Usage (In Ocaml utop - toplevel)

#### Setup (created) Ocaml Env
(in Powershell)


```
& opam env --switch=4.14.0 --shell=powershell | Invoke-Expression
dune clean
dune build
dune exec bin/fl_prover.exe
utop
```


Close the Ocaml utop with ctrl+D


#### Example Usage

(ignore the first '#'s)


```
# #require "floyd_hoare_solver";;
# open Floyd_hoare_solver;;
# let pre  = Compare (Eq, Var "x", Const 0);;
# let cmd  = Assign ("x", BinOp (Add, Var "x", Const 1));;
# let post = Compare (Eq, Var "x", Const 1);;
# prove pre cmd post;;
- : bool = true
(ctrl-d to exit)
```


#!/usr/bin/env bash

# some useful commands for ocaml (use bash for now)
# opam init
# eval (& opam env)
# opam --version
#
#
# # create or switch to ocaml compiler
# opam switch show
# # if none or not recent, pick one
# opam switch create 4.14.0
# eval $(opam env)
#
# # install core tools
# opam install dune utop merlin ocaml-lsp-server

# # get started
# & opam env --switch=4.14.0 --shell=powershell | Invoke-Expression
# dune clean
# dune build
# dune exec bin/fl_prover.exe

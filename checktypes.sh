#!/bin/bash


ocamlbuild -use-ocamlfind main.cmo state.cmo command.cmo gui.cmo
if [[ $? -ne 0 ]]; then
  cat <<EOF
===========================================================
WARNING

Your code currently does not compile.  You will receive
little to no credit for submitting this code. Check the
error messages above carefully to determine what is wrong.
See a consultant for help if you cannot determine what is
wrong.
===========================================================
EOF
  exit 1
fi



#/bin/bash

rm -f *.cm*

ocamlc -c Cnf.ml
ocamlc -c calcParser.mli
ocamlc -c calcLexer.ml
ocamlc -c calcParser.ml
ocamlc -c Ordinal.ml
ocamlc -c main.ml
ocamlc -o main calcLexer.cmo calcParser.cmo Cnf.cmo Ordinal.cmo main.cmo
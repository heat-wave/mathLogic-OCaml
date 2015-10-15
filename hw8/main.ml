open Cnf 
open Ordinal 
open CalcParser
open CalcLexer

let in_channel = open_in "test8.in";;
let o1, o2 = line lex (Lexing.from_string (input_line in_channel)) in
    print_string(if Cnf.cmp (Ordinal.ord2CNF o1) (Ordinal.ord2CNF o2) = 0 then "Equals\n" else "Not equals\n")

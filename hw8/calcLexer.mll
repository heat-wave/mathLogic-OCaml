{
open CalcParser
}

rule lex = parse
  | [' ' '\n' '\t'] { lex lexbuf }
  | '='				{ EQUALS }
  | 'w'				{ OMEGA }
  | '+'             { PLUS }
  | '^'				{ CARET }
  | '*'             { TIMES }
  | '('             { OPEN }
  | ')'             { CLOSE }
  | ['0'-'9']+ as s { INT(int_of_string s) }
  | eof             { EOF }

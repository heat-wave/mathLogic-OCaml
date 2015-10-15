type token =
    INT of int
  | OMEGA
  | PLUS
  | CARET
  | TIMES
  | OPEN
  | CLOSE
  | EOF
  | EQUALS
type ordinal =
    Add of ordinal * ordinal
  | Mul of ordinal * ordinal
  | Exp of ordinal * ordinal
  | Int of int
  | Omega
val yytransl_const : int array
val yytransl_block : int array
val yylhs : string
val yylen : string
val yydefred : string
val yydgoto : string
val yysindex : string
val yyrindex : string
val yygindex : string
val yytablesize : int
val yytable : string
val yycheck : string
val yynames_const : string
val yynames_block : string
val yyact : (Parsing.parser_env -> Obj.t) array
val yytables : Parsing.parse_tables
val line : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ordinal * ordinal

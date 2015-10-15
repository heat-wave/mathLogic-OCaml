type token =
  | INT of (int)
  | OMEGA
  | PLUS
  | CARET
  | TIMES
  | OPEN
  | CLOSE
  | EOF
  | EQUALS

open Parsing;;
let _ = parse_error;;
# 1 "calcParser.mly"
type ordinal = Add of ordinal * ordinal | Mul of ordinal * ordinal | Exp of ordinal * ordinal | Int of int | Omega
# 17 "calcParser.ml"
let yytransl_const = [|
  258 (* OMEGA *);
  259 (* PLUS *);
  260 (* CARET *);
  261 (* TIMES *);
  262 (* OPEN *);
  263 (* CLOSE *);
    0 (* EOF *);
  264 (* EQUALS *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\003\000\003\000\003\000\003\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\001\000\000\000\008\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\
\000\000\007\000"

let yydgoto = "\002\000\
\006\000\007\000"

let yysindex = "\255\255\
\016\255\000\000\000\000\000\000\016\255\000\000\001\255\020\255\
\016\255\016\255\016\255\016\255\000\000\254\254\007\255\007\255\
\016\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\001\000\007\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\003\000"

let yytablesize = 277
let yytable = "\001\000\
\006\000\010\000\011\000\009\000\010\000\011\000\005\000\008\000\
\012\000\004\000\010\000\014\000\015\000\016\000\017\000\018\000\
\003\000\004\000\000\000\000\000\000\000\005\000\009\000\010\000\
\011\000\000\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\006\000\000\000\006\000\
\006\000\005\000\000\000\005\000\004\000\005\000\005\000\000\000\
\004\000\004\000\009\000\010\000\011\000"

let yycheck = "\001\000\
\000\000\004\001\005\001\003\001\004\001\005\001\000\000\005\000\
\008\001\000\000\004\001\009\000\010\000\011\000\012\000\000\000\
\001\001\002\001\255\255\255\255\255\255\006\001\003\001\004\001\
\005\001\255\255\007\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\005\001\255\255\007\001\
\008\001\003\001\255\255\005\001\003\001\007\001\008\001\255\255\
\007\001\008\001\003\001\004\001\005\001"

let yynames_const = "\
  OMEGA\000\
  PLUS\000\
  CARET\000\
  TIMES\000\
  OPEN\000\
  CLOSE\000\
  EOF\000\
  EQUALS\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 18 "calcParser.mly"
           ( Omega )
# 156 "calcParser.ml"
               : 'ordinal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 19 "calcParser.mly"
                  ( Int _1 )
# 163 "calcParser.ml"
               : 'ordinal))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ordinal) in
    Obj.repr(
# 20 "calcParser.mly"
                     ( _2 )
# 170 "calcParser.ml"
               : 'ordinal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ordinal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ordinal) in
    Obj.repr(
# 21 "calcParser.mly"
                        ( Add(_1, _3) )
# 178 "calcParser.ml"
               : 'ordinal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ordinal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ordinal) in
    Obj.repr(
# 22 "calcParser.mly"
                        ( Mul(_1, _3) )
# 186 "calcParser.ml"
               : 'ordinal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ordinal) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ordinal) in
    Obj.repr(
# 23 "calcParser.mly"
                        ( Exp(_1, _3) )
# 194 "calcParser.ml"
               : 'ordinal))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ordinal) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ordinal) in
    Obj.repr(
# 26 "calcParser.mly"
                             ( (_1, _3) )
# 202 "calcParser.ml"
               : ordinal * ordinal))
(* Entry line *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let line (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ordinal * ordinal)
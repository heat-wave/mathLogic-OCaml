%{type ordinal = Add of ordinal * ordinal | Mul of ordinal * ordinal | Exp of ordinal * ordinal | Int of int | Omega %}

%token <int> INT
%token OMEGA
%token PLUS CARET TIMES OPEN CLOSE EOF
%token EQUALS

%start line
%type <ordinal * ordinal> line

%left PLUS
%left TIMES
%right CARET

%%

ordinal:
| OMEGA				{ Omega }
| INT             { Int $1 }
| OPEN ordinal CLOSE { $2 }
| ordinal PLUS ordinal  { Add($1, $3) }
| ordinal TIMES ordinal { Mul($1, $3) }
| ordinal CARET ordinal { Exp($1, $3) };

line:
| ordinal EQUALS ordinal EOF { ($1, $3) };
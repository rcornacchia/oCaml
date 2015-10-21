
%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE ASSIGN COMMA EOF
%token <int> LITERAL VARIABLE

%left COMMA
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr     { Binop($1, Add, $3) }
| expr MINUS  expr     { Binop($1, Sub, $3) }
| expr TIMES  expr     { Binop($1, Mul, $3) }
| expr DIVIDE expr     { Binop($1, Div, $3) }
| LITERAL              { Lit($1) }
| VARIABLE             { Var($1) }
| VARIABLE ASSIGN expr { Asn($1, $3) }
| expr COMMA expr      { Seq($1, $3) }

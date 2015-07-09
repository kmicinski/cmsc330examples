%start <Cekamlsol.Syntax.term> prog

%token <string> ID
%token <int> INT
%token EOF EOL DEF LPAREN RPAREN
%token PLUS MINUS TIMES DIVIDE
%token EQUALS
%token TRUE FALSE
%token IF THEN ELSE
%token LET IN
%token LARGER SMALLER EQLARGER EQSMALLER EQUAL NOTEQUAL
%token NOT AND OR
%token FUN TO
%left OR
%left AND
%nonassoc NOT
%nonassoc LARGER SMALLER EQLARGER EQSMALLER EQUAL NOTEQUAL
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc LPAREN
%nonassoc ATTRIB
%%

prog: 
  | t = term EOF { t }
  ;

term:
  | id = ID { Cekamlsol.Syntax.Var id }
  | FUN id = ID TO t = term { Cekamlsol.Syntax.Lam(id,t) }
  | LPAREN t = term RPAREN { t }
  | t1 = term t2 = term { Cekamlsol.Syntax.App(t1,t2) }
  | int = INT { Cekamlsol.Syntax.NumLit int }
  | TRUE { Cekamlsol.Syntax.BoolLit true }
  | FALSE { Cekamlsol.Syntax.BoolLit true }
  | t1 = term PLUS t2 = term { Cekamlsol.Syntax.Builtin(Plus,[t1;t2]) }
  | t1 = term MINUS t2 = term { Cekamlsol.Syntax.Builtin(Minus,[t1;t2]) }
  | LET id = ID EQUALS t1 = term IN t2 = term { Cekamlsol.Syntax.Let(id,t1,t2) }
  | IF t1 = term THEN t2 = term ELSE t3 = term { Cekamlsol.Syntax.Ifthenels(t1,t2,t3) }
  ; 

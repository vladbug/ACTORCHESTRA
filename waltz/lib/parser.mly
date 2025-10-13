%{
open Expressions
%}

%token<string> NUM BOOL LIDENT UIDENT
%token SEND WITH OMEGA THETA TOP BOTTOM
%token ARROW COLON SEMICOLON
%token EQ NEQ GT LT GTE LTE PLUS MINUS MULT DIV
%token NOT ANDALSO ORELSE
%token LPAR RPAR LBRACE RBRACE COMMA WILDCARD EOF

%left SEMICOLON
%left COLON
%left ORELSE
%left ANDALSO
%left EQ NEQ GT LT GTE LTE
%left PLUS MINUS
%left MULT DIV
%right NOT
%right OMEGA

%start program
%type <Expressions.atl_formula> program

%%

program:
  | formula EOF { $1 }

formula:
  | TOP                                     { Top }
  | BOTTOM                                  { Bottom }
  | signature COLON constraint_expr         { Signature($1, $3) }
  
  | formula SEMICOLON formula               { Sequence($1, $3) }
  | OMEGA LPAR formula RPAR                 { Omega($3) }
  | THETA LPAR formula RPAR                 { Theta($3) }

  | LPAR formula RPAR                       { $2 }

signature:
  | SEND LBRACE LIDENT ARROW LIDENT RBRACE WITH LBRACE pattern RBRACE
    { (* Ensure pattern inside WITH {...} is always treated as a tuple *)
      let tuple_pat = match $9 with
        | PList _ -> $9  (* Already a list/tuple, keep as is *)
        | p -> PList [p]  (* Single element, wrap in PList to make it a tuple *)
      in
      Send($3, $5, tuple_pat) 
    }

pattern:
  | basic_pattern                          { $1 }
  | pattern_list                           { PList($1) }

basic_pattern:
  | WILDCARD                               { PWildcard }
  | LIDENT                                 { PAtom($1) }
  | UIDENT                                 { PVar($1) }
  | LBRACE pattern RBRACE                  { $2 }

pattern_list:
  | basic_pattern COMMA basic_pattern      { [$1; $3] }
  | basic_pattern COMMA pattern_list       { $1 :: $3 }

constraint_expr:
  | TOP                                    { CTrue }
  | BOTTOM                                 { CFalse }
  | BOOL                                   { CBool(bool_of_string $1) }
  | LIDENT                                 { CVar($1) }
  | UIDENT                                 { CVar($1) }
  | NUM                                    { CNum(int_of_string $1) }
  
  | constraint_expr PLUS constraint_expr   { CBinOp(Add, $1, $3) }
  | constraint_expr MINUS constraint_expr  { CBinOp(Sub, $1, $3) }
  | constraint_expr MULT constraint_expr   { CBinOp(Mult, $1, $3) }
  | constraint_expr DIV constraint_expr    { CBinOp(Div, $1, $3) }
  | constraint_expr EQ constraint_expr     { CBinOp(Eq, $1, $3) }
  | constraint_expr NEQ constraint_expr    { CBinOp(Neq, $1, $3) }
  | constraint_expr GT constraint_expr     { CBinOp(Gt, $1, $3) }
  | constraint_expr LT constraint_expr     { CBinOp(Lt, $1, $3) }
  | constraint_expr GTE constraint_expr    { CBinOp(Gte, $1, $3) }
  | constraint_expr LTE constraint_expr    { CBinOp(Lte, $1, $3) }
  | constraint_expr ANDALSO constraint_expr { CBinOp(AndOp, $1, $3) }
  | constraint_expr ORELSE constraint_expr  { CBinOp(OrOp, $1, $3) }
  
  | NOT constraint_expr                    { CNot($2) }
  
  | LPAR constraint_expr RPAR              { $2 }
{
  open Parser
}

let space = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let num = ['0' - '9']+
let bool = "true" | "false"

rule token = parse
  | space               { token lexbuf } (* Skip whitespace *)
  (* WALTZ Keywords *)
  | "send"              { SEND }
  | "WITH"              { WITH }
  | "OMEGA"             { OMEGA }
  | "THETA"             { THETA }
  | "TOP"               { TOP }
  | "BOTTOM"            { BOTTOM }
  (* Operators *)
  | "->"                { ARROW }
  | ":"                 { COLON }
  | ";"                 { SEMICOLON }
  (* Comparison operators *)
  | "=="                { EQ }
  | "=/="               { NEQ }
  | ">="                { GTE }
  | "=<"                { LTE }
  | ">"                 { GT }
  | "<"                 { LT }
  (* Arithmetic operators *)
  | "+"                 { PLUS }
  | "-"                 { MINUS }
  | "*"                 { MULT }
  | "/"                 { DIV }
  (* Logical operators *)
  | "not" | "¬"         { NOT }
  | "AND"           { ANDALSO }
  | "OR"            { ORELSE }
  (* Delimiters *)
  | "("                 { LPAR }
  | ")"                 { RPAR }
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }
  | ","                 { COMMA }
  | "_"                 { WILDCARD }
  (* Literals *)
  | ['0'-'9']+ as n     { NUM n }
  | "true" | "false" as b { BOOL b }
  (* Identifiers *)
  | ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { LIDENT id }
  | ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { UIDENT id }
  | eof                 { EOF }
  | _ as c              { failwith ("lexing: unrecognized character " ^ Char.escaped c) }
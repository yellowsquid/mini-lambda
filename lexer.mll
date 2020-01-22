(* Compiler Construction - Minimal Lambda Language
 *
 * This file defines the lexer of the language: a list of
 * regexes which produce tokens defined by and consumed by
 * the parser.
 *)

{
  open Parser
  open Lexing
  exception Error of int * int * char
}

rule token = parse
  | ['\n'] { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t'] { token lexbuf }
  | eof { EOF }
  | '+' { PLUS }
  | '-' { MINUS }
  | "==" { EQUAL }
  | "!=" { NEQUAL }
  | '!' { INVERT }
  | "&&" { AND }
  | "||" { OR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | ";" { SEMI }
  | "\\" { LAMBDA }
  | "->" { ARROW }
  | "func" { FUNC }
  | "if" { IF }
  | "else" { ELSE }
  | "return" { RETURN }
  | "<-" { BIND }
  | "true" { TRUE }
  | "false" { FALSE }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* {
      IDENT (Lexing.lexeme lexbuf)
    }
  | ['_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']* {
      IGNORE (Lexing.lexeme lexbuf)
    }
  | ['0' - '9']+ as str { INT(int_of_string str) }
  | _ as c {
    let { pos_lnum; pos_cnum; pos_bol; _} = Lexing.lexeme_start_p lexbuf in
    raise (Error(pos_lnum, pos_cnum - pos_bol + 1, c))
  }

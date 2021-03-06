(* Compiler Construction - Minimal Lambda Language
 *
 * Entry point to the compiler: parses arguments and runs the
 * code generation pipeline. In case of an error, a diagnostic
 * is emitted pointing to a location in the source code.
 *)

let compile = ref true
let in_chan = ref stdin
let out_chan = ref stdout
let interpreter = ref I0.interpret
let debug = ref false

let usage = "usage: " ^ Sys.argv.(0) ^ "[-o out] input"

let speclist =
  [ ( "-o"
    , Arg.String (fun s -> out_chan := open_out s)
    , ": output stream"
    )
  ; ( "-g"
    , Arg.Unit (fun () -> debug := true)
    , ": debug comments"
    )
  ; ( "-i"
    , Arg.Int
        (fun i ->
          compile := false;
          interpreter := match i with
                         | 1 -> I1.interpret
                         | 2 -> I2.interpret
                         | 3 -> I3.interpret
                         | 4 -> I4.interpret
                         | 5 -> I5.interpret
                         | 6 -> I6.interpret
                         | _ -> I0.interpret )
    , ": use interpretter"
    )
  ]

let () =
  Arg.parse speclist (fun s -> in_chan := open_in s) usage;
  let lexbuf = Lexing.from_channel !in_chan in
  try
    let ast = Parser.program Lexer.token lexbuf in
    let typed_ast = Typing.check ast in
    Analysis.analyse typed_ast;
    if !compile then
      let ir = Ir_lowering.lower !debug typed_ast in
      let lir = Lir_lowering.lower !debug ir in
      Backend.compile !debug !out_chan lir
    else
      !interpreter !debug typed_ast
  with
  | Lexer.Error (lnum, cnum, chr) ->
     Printf.eprintf "(%d:%d) lexer error: invalid character '%c'\n"
       lnum
       cnum
       chr;
     exit 1
  | Parser.Error ->
     let pos = Lexing.lexeme_start_p lexbuf in
     Printf.eprintf "(%d:%d) parser error: invalid token '%s'\n"
       pos.Lexing.pos_lnum
       (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
       (Lexing.lexeme lexbuf);
     exit 1
  | Analysis.Error (pos, msg) ->
     Printf.eprintf "(%d:%d) semantic error: %s\n"
       pos.Lexing.pos_lnum
       (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
       msg;
     exit 1
  | Typing.Error (pos, msg) ->
     Printf.eprintf "(%d:%d) type error: %s\n"
       pos.Lexing.pos_lnum
       (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)
       msg;
     exit 1

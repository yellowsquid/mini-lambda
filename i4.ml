open Typed_ast

type directive
  = Add
  | Sub
  | Equal
  | And
  | Or
  | Invert
  | PushFunc of int
  | PushLocal of int
  | PushArg of int
  | Push of value
  | Pop
  | PopEnv
  | Seq
  (* Params * Locals * Body *)
  | Capture of int * int * directive list
  | Builtin of string
  | Call of int
  | Bind of int
  | If of directive list * directive list
and value
  = None
  | Unit
  | Bool of bool
  | Int of int
  (* Params * Locals * Body *)
  | Func of int * value list  * directive list
and env =
  { locals: int
  ; stack: value ref array
  }

module FuncMap = Map.Make(String)

let list_sep ppf _ = Format.fprintf ppf ",@ "

let rec pp_directive ppf directive = match directive with
  | Add -> Format.fprintf ppf "Add"
  | Sub -> Format.fprintf ppf "Sub"
  | Equal -> Format.fprintf ppf "Equal"
  | And -> Format.fprintf ppf "And"
  | Or -> Format.fprintf ppf "Or"
  | Invert -> Format.fprintf ppf "Invert"
  | PushFunc i -> Format.fprintf ppf "PushFunc(%d)" i
  | PushLocal i -> Format.fprintf ppf "PushLocal(%d)" i
  | PushArg i -> Format.fprintf ppf "PushArg(%d)" i
  | Push v ->
     Format.fprintf ppf "@[<4>Push(";
     pp_value ppf v;
     Format.fprintf ppf ")@]"
  | Pop -> Format.fprintf ppf "Pop"
  | PopEnv -> Format.fprintf ppf "PopEnv"
  | Seq -> Format.fprintf ppf "Seq"
  | Capture (params, captures, body) ->
     Format.fprintf ppf "@[<4>Capture(%d,@ %d,@ " params captures;
     pp_directive_list ppf body;
     Format.fprintf ppf ")@]"
  | Builtin name -> Format.fprintf ppf "Builtin(%s)" name
  | Call args -> Format.fprintf ppf "Call(%d)" args
  | Bind id -> Format.fprintf ppf "Bind(%d)" id
  | If (tblock, fblock) ->
     Format.fprintf ppf "@[<4>If(";
     Format.pp_print_list ~pp_sep:list_sep pp_directive_list ppf [tblock; fblock];
     Format.fprintf ppf "@])@]"
and pp_directive_list ppf directives =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_directive ppf directives;
  Format.fprintf ppf ")@]"
and pp_value ppf value = match value with
  | None -> Format.fprintf ppf "None"
  | Unit -> Format.fprintf ppf "()"
  | Bool b -> Format.fprintf ppf "%B" b
  | Int i -> Format.fprintf ppf "%d" i
  | Func (params, locals, body) ->
     Format.fprintf ppf "@[<4>Func(%d,@ " params;
     pp_value_list ppf locals;
     list_sep ppf ();
     pp_directive_list ppf body;
     Format.fprintf ppf ")@]"
and pp_value_list ppf values =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_value ppf values;
  Format.fprintf ppf ")@]"

let pp_env ppf env =
  Format.fprintf ppf "@[<1>(%d,@ " env.locals;
  pp_value_list ppf (Array.to_list (Array.map (!) env.stack));
  Format.fprintf ppf ")@]"

let pp_env_list ppf envs =
  Format.fprintf ppf "@[<1>(";
  Format.pp_print_list ~pp_sep:list_sep pp_env ppf envs;
  Format.fprintf ppf ")@]"

let print_int env = match !(env.stack.(0)) with
  | Int x -> print_int x; print_newline(); Unit
  | _ -> failwith "type mismatch"

let print_bool env = match !(env.stack.(0)) with
  | Bool b -> print_string (string_of_bool b); print_newline (); Unit
  | _ -> failwith "type mismatch"

let input_int _ = Int (read_int ())

let builtins = FuncMap.of_seq (List.to_seq [ "print_int", (print_int, 1)
                                           ; "print_bool", (print_bool, 1)
                                           ; "input_int", (input_int, 0)])

let funcs = ref (Array.of_list [])

let rec flatten_expr acc expr = match expr with
  | FuncExpr (_, id) -> PushFunc id :: acc
  | EnvExpr (_, id) -> PushLocal id :: acc
  | BoundExpr (_, id) -> PushLocal id :: acc
  | ArgExpr (_, id) -> PushArg id :: acc
  | IntExpr (_, i) -> Push (Int i) :: acc
  | BoolExpr (_, b) -> Push (Bool b) :: acc
  | AddExpr (_, lhs, rhs) -> flatten_expr (flatten_expr (Add :: acc) rhs) lhs
  | SubExpr (_, lhs, rhs) -> flatten_expr (flatten_expr (Sub :: acc) rhs) lhs
  | EqualExpr (_, lhs, rhs) -> flatten_expr (flatten_expr (Equal :: acc) rhs) lhs
  | AndExpr (_, lhs, rhs) -> flatten_expr (flatten_expr (And :: acc) rhs) lhs
  | OrExpr (_, lhs, rhs) -> flatten_expr (flatten_expr (Or :: acc) rhs) lhs
  | InvertExpr (_, e) -> flatten_expr (Invert :: acc) e
  | LambdaExpr (_, params, captures, body) ->
     let acc' = Capture (params, Array.length captures, flatten_expr [] body) :: acc in
     List.fold_left flatten_expr acc' (List.rev (Array.to_list captures))
  | CallExpr (_, callee, args) ->
     let acc' = Call (Array.length args) :: PopEnv :: acc in
     flatten_expr (List.fold_left flatten_expr acc' (List.rev (Array.to_list args))) callee

let rec flatten_stmt acc stmt = match stmt with
  | ReturnStmt (_, e) -> flatten_expr acc e
  | ExprStmt (_, e) -> flatten_expr (Pop :: Push None :: acc) e
  | BindStmt (_, id, e) -> flatten_expr (Bind id :: acc) e
  | IfStmt (_, cond, tblock, fblock) ->
     flatten_expr (If (make_block tblock, make_block fblock) :: acc) cond
and make_block stmts = List.flatten (List.map (flatten_stmt [Seq]) stmts) @ [Push None]

let rec take_rev n acc stack = match n, stack with
  | 0, _ -> acc, stack
  | _, v :: rest -> take_rev (n - 1) (v :: acc) rest
  | _, _ -> failwith "stack too short"

let step directives values envs = match directives, values, envs with
  | [], values, envs -> [], values, envs
  | Add :: rest, Int b :: Int a :: values', _ -> rest, Int (a + b) :: values', envs
  | Sub :: rest, Int b :: Int a :: values', _ -> rest, Int (a - b) :: values', envs
  | Equal :: rest, Int b :: Int a :: values', _ -> rest, Bool (a = b) :: values', envs
  | Equal :: rest, Bool b :: Bool a :: values', _ -> rest, Bool (a = b) :: values', envs
  | And :: rest, Bool b :: Bool a :: values', _ -> rest, Bool (a && b) :: values', envs
  | Or :: rest, Bool b :: Bool a :: values', _ -> rest, Bool (a || b) :: values', envs
  | Invert :: rest, Bool b :: values', _ -> rest, Bool (not b) :: values', envs

  | PushFunc id :: rest, _, _ -> rest, Array.get !funcs id :: values, envs
  | PushLocal id :: rest, _, env :: _ -> rest, !(Array.get env.stack id) :: values, envs
  | PushArg id :: rest, _, env :: _ ->
     rest, !(Array.get env.stack (id + env.locals)) :: values, envs
  | Push v :: rest, _, _ -> rest, v :: values, envs
  | Pop :: rest, _ :: values', _ -> rest, values', envs
  | PopEnv :: rest, _, _ :: envs' -> rest, values, envs'
  | Seq :: rest, None :: values', _ -> rest, values', envs
  | Seq :: _ :: rest, _, _ -> rest, values, envs

  | Capture (params, captures, body) :: rest, _, _ ->
     let captures', values' = take_rev captures [] values in
     rest, Func (params, captures', body) :: values', envs
  | Builtin name :: rest, _, env :: _ ->
     rest, (fst (FuncMap.find name builtins)) env :: values, envs

  | Call args :: rest, _, _ ->
     let args', values' = take_rev args [] values in
     (match values' with
      | Func (params, locals, body) :: values'' when params = args ->
         let env' = { stack = Array.of_list (List.map ref (locals @ args'))
                    ; locals = List.length locals
                    } in
         body @ rest, values'', env' :: envs
      | _ -> failwith "type mismatch"
     )

  | Bind id :: rest, v :: values', env :: _ ->
     (Array.get env.stack id) := v;
     rest, None :: values', envs
  | If (tblock, _) :: rest, Bool true :: values', _ -> tblock @ rest, values', envs
  | If (_, fblock) :: rest, Bool false :: values', _ -> fblock @ rest, values', envs
  | _, _, _ -> failwith "type mismatch"

let stage = ref 0

let driver debug env directives =
  let rec iter (directives, values, envs) =
    if debug then begin
        incr stage;
        Format.fprintf Format.std_formatter "@[<4>Step %d: (@," !stage;
        pp_directive_list Format.std_formatter directives;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_value_list Format.std_formatter values;
        Format.fprintf Format.std_formatter ",@ @,";
        pp_env_list Format.std_formatter envs;
        Format.fprintf Format.std_formatter ")@]@\n" end;
    match directives with
    | [] -> values
    | _ -> iter (step directives values envs) in
  iter (directives, [], [env])


let interpret_func func =
  if Option.is_none func.body then
    if FuncMap.mem func.name builtins then
      let _, params = FuncMap.find func.name builtins in
      Func (params, [], [Builtin func.name; Seq; Push None])
    else
      failwith "built-in has no definition"
  else
    let locals = List.init func.num_locals (fun _ -> None) in
    Func (func.num_params, locals, make_block (Option.get func.body))

let interpret debug program =
  let program' = Array.concat (Array.to_list program) in
  Array.sort (fun a b -> compare a.id b.id) program';
  funcs := Array.map interpret_func program';
  let main = List.find (fun f -> f.name = "main") (Array.to_list program') in
  let env = { stack = Array.init main.num_locals (fun _ -> ref None)
            ; locals = main.num_locals
            } in
  match Array.get !funcs main.id with
  | Func(0, _, body) ->
     (match driver debug env body with
      | [None] -> ()
      | _ -> failwith "type mismatch")
  | _ -> failwith "main not a function"

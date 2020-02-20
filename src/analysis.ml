open Typed_ast

exception Error of loc * string

type return
  = Passthrough
  | Return
  | Break of int
  | Continue of int
  | Branch of return list

let rec string_of_return v = match v with
  | Passthrough -> "->"
  | Return -> "!"
  | Break id -> Printf.sprintf "break(%d)" id
  | Continue id -> Printf.sprintf "continue(%d)" id
  | Branch l -> Printf.sprintf "branch(%s)" (String.concat ", " (List.map string_of_return l))

(* Helper functions for branch set. *)
let set_remove l v =
  let rec iter acc rest = match rest with
    | [] -> acc
    | x :: rest -> if x = v then iter acc rest else iter (x :: acc) rest in
  iter [] l
let set_check_remove l v = List.mem v l, set_remove l v
let rec set_merge l m = match l with
  | [] -> m
  | v :: rest ->
     if List.mem v m
     then set_merge rest m
     else set_merge rest (v :: m)
let set_add l v =
  if List.mem v l
  then l
  else v :: l

let set_to_branch l = match l with
  | [] -> failwith "can't have empty return"
  | [v] -> v
  | vs -> Branch vs

let make_branch a b = match a, b with
  | Branch l, Branch m -> set_to_branch (set_merge l m)
  | Branch l, x -> set_to_branch (set_add l x)
  | x, Branch l -> set_to_branch (set_add l x)
  | x, y -> set_to_branch (List.fold_left set_add [] [x; y])

let merge_return a b = match a, b with
  | Passthrough, x -> x
  | Return, _ -> Return
  | Break id, _ -> Break id
  | Continue id, _ -> Continue id
  | Branch l, Branch m -> set_to_branch (set_merge (set_remove l Passthrough) m)
  | Branch l, x -> set_to_branch (set_add (set_remove l Passthrough) x)

let rec returns stmt = match stmt with
  | ReturnStmt _ -> Return
  | ExprStmt _ -> Passthrough
  | BindStmt _ -> Passthrough
  | MatchStmt (_, _, cases) ->
     cases |> List.map case_returns |> List.fold_left set_add [] |> set_to_branch
  | IfStmt (_, _, tblock, fblock) ->
     let tblock' = block_return tblock in
     let fblock' = block_return fblock in
     make_branch tblock' fblock'
  | WhileStmt (_, id, _, lblock, eblock) ->
     let lblock' = block_return lblock in
     let eblock' = block_return eblock in
     (match lblock' with
      | Passthrough -> eblock'
      | Return -> make_branch Return eblock'
      | Break id' when id' = id -> make_branch Passthrough eblock'
      | Break id' -> make_branch (Break id') eblock'
      | Continue id' when id' = id -> eblock'
      | Continue id' -> make_branch (Continue id') eblock'
      | Branch l ->
         let break, l' = set_check_remove l (Break id) in
         let continue, l'' = set_check_remove l' (Continue id) in
         let passthrough, l3 = set_check_remove l'' Passthrough in
         let l4 = if break then set_add l3 Passthrough else l3 in
         let l5 = if continue || passthrough
                  then match eblock' with
                       | Branch m -> set_merge l4 m
                       | x -> set_add l4 x
                  else l4 in
         set_to_branch l5
     )
  | ContinueStmt (_, id) -> Continue id
  | BreakStmt (_, id) -> Break id
and block_return stmts = List.fold_left merge_return Passthrough (List.map returns stmts)
and case_returns (_, _, stmts) = block_return stmts

let analyse program =
  Array.iter (Array.iter (fun func ->
      if Option.is_some func.body then
        match block_return (Option.get func.body) with
        | Passthrough -> ()
        | Return -> ()
        | x -> raise (Error (func.loc, "not all branches return: " ^ (string_of_return x)))
    )) program

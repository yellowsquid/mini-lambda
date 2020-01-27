type bin_op
  = Add
  | Sub
  | Equal
  | And
  | Or

type unary_op
  = Invert

let string_of_bin_op op = match op with
  | Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | And -> "&&"
  | Or -> "||"

let string_of_unary_op op = match op with
  | Invert -> "!"

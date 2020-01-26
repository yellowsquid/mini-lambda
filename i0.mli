exception Error of Typed_ast.loc * string

val interpret : bool -> Typed_ast.program -> unit

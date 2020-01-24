exception Error of Typed_ast.loc * string

val interpret : Typed_ast.program -> bool -> unit

open Syntax

val parse : string -> decls
val parse_from_string : string -> decls
val get_includes : string -> string list 
val get_all_source_filenames : string -> string list
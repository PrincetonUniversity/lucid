(*pragmas that are used throughout the compiler. 
  Generally, a pragma is attached to some node in the
  syntax by an earlier pass and then used in a later 
  pass. For example, there's a pragma that function 
  inlining puts on a local variable declaration that 
  indicates it is safe to not initialize that variable 
  when translating to P4 or C. *)

type t = 
| PNoInitLocal 
  (* this statement declares 
      a local variable that does not need to be 
      initialized.  *)
| PStringPrag of string * string list
  (* PStringPrag holds a pragma name string and 
     a list of string arguments. This is mainly 
     for pragmas that are temporary or in development. 
     Once a pragma is stable, it should be given a 
    proper constructor. *)

val sprag : string -> string list -> t

val find_sprag : string -> string list -> t list -> t option

val exists_pnolocal : t list -> bool

val exists_sprag : string -> string list -> t list -> bool

val to_string : t -> string

val to_strings : t list -> string
(* Program Variables *)

type t = string * int [@@deriving show]

val create : string -> t
val fresh : string -> t
val reset : unit -> unit
val name : t -> string
val to_id : string * int -> t
val from_id : t -> string * int
val to_string : t -> string
val to_string_delim : string -> t -> string
val prepend_string : string -> string * int -> string * int
val append_string : string -> string * int -> string * int
val refresh : t -> t
val freshen : t -> t (* Alias for refresh *)
val fresh_name : string -> t
val concat_names : string -> t -> t -> t

(* Inverse of to_string. Do not use for other values. *)
val of_id_string : string -> t
val equal : t -> t -> bool

(* Alias for equal TODO: Refactor this out*)
val equals : t -> t -> bool
val equal_names : t -> t -> bool
val compare : t -> t -> int

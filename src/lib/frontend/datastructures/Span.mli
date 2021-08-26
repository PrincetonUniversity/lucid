type t =
  { fname : string
  ; start : int
  ; finish : int
  ; spid : int
  }
[@@deriving show, ord]

val extend : t -> t -> t
val default : t
val to_string : t -> string

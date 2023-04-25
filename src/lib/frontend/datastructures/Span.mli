type cid = Cid.t
type tcid = TaggedCid.t
type t =
  { fname : string
  ; start : int
  ; finish : int
  ; spid : int
  ; global_created_in_src : tcid option
  }
(* [@@deriving show, ord] *)
val default : t
val create : string -> int -> int -> t
val extend : t -> t -> t
val to_string : t -> string

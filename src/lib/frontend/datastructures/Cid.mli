(* Compound Identifiers *)
type id = Id.t

type t =
  | Id of id
  | Compound of id * t

(* Constructors *)

val create : string list -> t
val create_ids : Id.t list -> t
val create_ids_rev : Id.t list -> t
val fresh : string list -> t
val id : Id.t -> t
val compound : Id.t -> t -> t
val str_cons : string -> t -> t
val concat : t -> t -> t
val from_string : string -> t

(* Destructors *)

val to_string : t -> string
val to_string_delim : string -> t -> string
val names : t -> string list
val to_id : t -> Id.t
val to_ids : t -> Id.t list
val to_ids_prefix : t -> Id.t * Id.t list
val first_id : t -> Id.t
val last_id : t -> Id.t
val tl : t -> t

(* Operations *)

val compare : t -> t -> int
val equal : t -> t -> bool
val equals : t -> t -> bool
val equal_names : t -> t -> bool
val lookup_opt : (t * 'a) list -> t -> 'a option
val lookup : (t * 'a) list -> t -> 'a
val exists : (t * 'a) list -> t -> bool
val replace : (t * 'a) list -> t -> 'a -> (t * 'a) list
val remove : (t * 'a) list -> t -> (t * 'a) list
val modify_tail : (id -> id) -> t -> t
val freshen_last : t -> t

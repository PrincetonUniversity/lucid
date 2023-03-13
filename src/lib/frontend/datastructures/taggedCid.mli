(* compound identifiers, with tags / annotations *)

type cid = Cid.t

type tagval = 
  | TagStr of string
  | TagInt of int

type t = {
  tcid : cid;
  tparent : t option;
  tag  : string option;
  tags : (string * tagval) list;
}

(* constructors *)
val cid : cid -> t
val id : Id.t -> t
val tagged_cid : cid -> string -> t
val tagged_id : Id.t -> string -> t

val create : string list -> t
val create_ids : Id.t list -> t
val create_tagged : string list -> string -> t
val create_ids_tagged : Id.t list -> string -> t

(* destructors *)
val to_cid : t -> cid
val to_id : t -> Id.t

val tagval_to_string : tagval -> string

val to_string : t -> string

(* modifiers *)
(* update type tag *)
val tytag : t -> string -> t
(* update index tag *)
val idxtag : t -> int -> t

val ty : t -> string

val equals : t -> t -> bool

val tcid_ancestors : t -> t list

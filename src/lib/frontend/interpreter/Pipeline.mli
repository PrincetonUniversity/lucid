open Syntax

type t

val empty : unit -> t

(* Takes a list of (width * length) pairs -- width is the number of bits in each entry,
   length is the number of entries *)
val of_globals : (int * int) list -> t

(* Returns a _new_ pipeline with one more stage *)
val append_stage : width:int -> length:int -> t -> t

(* Reset stage counter to 0. Should be done at the beginning of each handler *)
val reset_stage : t -> unit

(* Creates a deep copy of the pipeline *)
val copy : t -> t
val length : t -> int
val to_string : ?pad:string -> t -> string

(* Updates the given stage at the given index by applying setop, and returns the
   result of getop applied to the original value. Increments stage counter. *)
val update:
     stage:int
  -> idx:int
  -> getop:(zint -> 'a)
  -> setop:(zint -> zint)
  -> t
  -> 'a

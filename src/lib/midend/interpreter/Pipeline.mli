open Syntax

type t

val empty : unit -> t

(* Takes a list of (width * length, is_pair) triple -- width is the number of bits in each entry,
   length is the number of entries, and is_pair indicates that this is a pair array, with both halves
   having width bits *)
val of_globals : (int * int * bool) list -> t

(* Returns a _new_ pipeline with one more stage *)
val append_stage : width:int -> length:int -> pair:bool -> t -> t

(* Reset stage counter to 0. Should be done at the beginning of each handler *)
val reset_stage : t -> unit

(* Creates a deep copy of the pipeline *)
val copy : t -> t
val length : t -> int
val to_string : ?pad:string -> t -> string

(* Updates the given stage at the given index by applying setop, and returns the
   result of getop applied to the original value. Increments stage counter. Only
   works on non-pair arrays *)
val update:
     stage:int
  -> idx:int
  -> getop:(zint -> 'a)
  -> setop:(zint -> zint)
  -> t
  -> 'a
[@@ocamlformat "disable"]

(* Same as update, but takes a complex memop, and works on either kind of array *)
val update_complex:
     stage:int
  -> idx:int
  -> memop:(zint -> zint -> zint * zint * 'a)
  -> t
  -> 'a
[@@ocamlformat "disable"]

open CoreSyntax

type obj

(* create an array object to place in pipeline *)
val mk_array : id:Id.t -> width:int -> length:int -> pair:bool -> obj

val mk_table : id:Id.t -> length:int -> obj

type t

val empty : unit -> t

(* Returns a _new_ pipeline with one more stage *)
val append : t -> obj -> t

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

(* Same as update, but takes a complex memop, and works on either kind of array. *)
val update_complex:
     stage:int
  -> idx:int
  -> memop:(zint -> zint -> zint * zint * 'a)
  -> t
  -> 'a
[@@ocamlformat "disable"]

(* get mutable entries from table at stage *)
val get_table_entries:
      stage:int
   -> t
   -> case list
[@@ocamlformat "disable"]   

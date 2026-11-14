include LibraryInterface.TypeInterface

(* create the table, adding it to a pipeline in a switch *)
val create_ctor : InterpSwitch.state -> InterpSwitch.ival list -> Pipeline.t
(* look up a key in the table and run the matching entry's action *)
val lookup_fun : InterpSwitch.code

(* the priority of every entry in an exact table *)
val exact_priority : int

include LibraryInterface.TypeInterface

(* create the table, adding it to a pipeline in a switch *)
val create_ctor : InterpSwitch.state -> InterpSwitch.ival list -> Pipeline.t

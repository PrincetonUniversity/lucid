module T = ANSITerminal

exception Error of string

(* read_files initializes info about the input files for position printing *)
val read_files : string list -> unit
val error : string -> 'a
val warning : string -> unit
val report : string -> unit
val show_message : string -> T.color -> string -> unit
val error_position : Span.t -> string -> 'a
val warning_position : Span.t -> string -> unit
val report_position : Span.t -> string -> unit
val get_start_position : Span.t -> (int * int) option
val get_end_position : Span.t -> (int * int) option

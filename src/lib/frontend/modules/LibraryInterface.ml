open InterpState
open Syntax

(* All libraries should have this interface *)
module type BasicInterface = sig
  val module_id : Id.t
  val defs : State.global_fun list

  val signature:
    Id.t * (Id.t * sizes * ty) list * State.global_fun list * (Cid.t * func_ty) list
  [@@ocamlformat "disable"]
end

(* Libraries which define a type should have this interface *)
module type TypeInterface = sig
  include BasicInterface

  val t_id : Cid.t
  val constructors : (Cid.t * func_ty) list
  val sizes : int (* Number of polymorphic size arguments the type takes *)
  val ty_args : int (* Number of polymorphic type arguments the type takes *)
  val global : bool (* Whether this type is global or not *)
end

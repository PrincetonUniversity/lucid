include LibraryInterface.TypeInterface

val type_checker : Syntax.decl list -> Syntax.decl list


(* helpers for interp and core -- these will eventually be 
   eliminated, but smooth the conversion from custom table syntax 
   to the standardized Table module. *)

val function_cids : Cid.t list
   
val is_tbl_ty : CoreSyntax.raw_ty -> bool

type core_tbl_ty = 
{ tkey_sizes : CoreSyntax.size list
; tparam_tys : CoreSyntax.ty list
; tret_tys : CoreSyntax.ty list
}
;;
type core_tbl_def = 
{ tid : CoreSyntax.id (* for convenience *)
; tty : CoreSyntax.ty
; tactions : CoreSyntax. exp list
; tsize : CoreSyntax.exp
; tdefault : CoreSyntax.cid * CoreSyntax.exp list
};;

type core_tbl_match = 
{
  tbl : CoreSyntax.exp; 
  keys : CoreSyntax.exp list;
  args : CoreSyntax.exp list;
  outs : Id.t list; 
  out_tys : CoreSyntax.ty list option;
}

val tname_to_ttable : CoreSyntax.raw_ty -> core_tbl_ty

val dglobal_params_to_tbl_def : CoreSyntax.id -> CoreSyntax.exp -> core_tbl_def
val tbl_def_to_econstr : core_tbl_def -> CoreSyntax.exp

val s_to_tbl_match : CoreSyntax.s -> core_tbl_match
val s_to_tbl_match_opt : CoreSyntax.s -> core_tbl_match option
val tbl_match_to_s : core_tbl_match -> CoreSyntax.s

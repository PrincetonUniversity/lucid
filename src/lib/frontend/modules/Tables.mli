include LibraryInterface.TypeInterface

val type_checker : Syntax.decl list -> Syntax.decl list


val is_tbl_ty : CoreSyntax.raw_ty -> bool

type core_tbl_ty = 
{ tkey_sizes : CoreSyntax.size list
; tparam_tys : CoreSyntax.ty list
; tret_tys : CoreSyntax.ty list
}
;;


val tname_to_ttable : CoreSyntax.raw_ty -> core_tbl_ty
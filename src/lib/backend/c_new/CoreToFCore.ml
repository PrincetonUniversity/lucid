module C = CoreSyntax
module F = FCoreSyntax

let err = Console.error ;;

let size_to_ty = function 
  | C.Sz(sz) -> F.ty@@F.TInt(sz)
  | C.Szs(szs) -> F.ty@@F.TRecord{labels=None; ts=List.map (fun sz -> F.ty@@F.TInt(sz)) szs}
;;

let rec translate_raw_ty (raw_ty : C.raw_ty) : F.raw_ty = 
  match raw_ty with 
  | C.TBool -> F.TBool
  | C.TInt(Sz sz) ->  F.TInt(sz)
  | C.TInt(_) -> err "TInt size should be a singleton"
  | C.TEvent -> F.TEvent
  | C.TFun {arg_tys; ret_ty} -> 
    let fty : F.func_ty = {
      F.arg_tys = List.map translate_ty arg_tys; 
      F.ret_ty = translate_ty ret_ty;
      F.func_kind = FNormal;}
    in
    F.TFun fty
  | C.TName(cid, sizes, is_global) -> 
    (* convert sizes, which may be singletons or lists of ints, 
       into int types or tuple types *)
    F.TName(cid, List.map size_to_ty sizes, is_global)
  | C.TMemop(n_args, Sz(arg_size)) -> 
    let arg_tys = List.init n_args (fun _ -> F.ty@@F.TInt(arg_size)) in
    let ret_ty = F.ty@@F.TInt(arg_size) in
    let fty : F.func_ty = {
      F.arg_tys = arg_tys; 
      F.ret_ty = ret_ty;
      F.func_kind = FMemop;}
    in
    F.TFun fty
  | C.TMemop(_, _) -> err "TMemop size should be a singleton"
  | C.TAction(aty) -> F.TFun(translate_acn_ty aty)
  | C.TActionConstr{aconst_param_tys; aacn_ty} -> 
    (* an action constructor is a normal function that returns an action *)
    let fty : F.func_ty = {
      F.arg_tys = List.map translate_ty aconst_param_tys; 
      F.ret_ty = F.ty@@F.TFun(translate_acn_ty aacn_ty);
      F.func_kind = FNormal;}
    in
    F.TFun fty
  | C.TRecord(id_rawty_pairs) -> 
    let ids, raw_tys = List.split id_rawty_pairs in
    let raw_tys = List.map translate_raw_ty raw_tys in
    let tys = List.map F.ty raw_tys in
    F.TRecord{labels=Some ids; ts=tys}
  | C.TTuple(raw_tys) ->
    let raw_tys = List.map translate_raw_ty raw_tys in
    let tys = List.map F.ty raw_tys in
    F.TRecord{labels=None; ts=tys}
  | C.TGroup -> F.TInt(-1)
  | C.TPat(Sz(sz)) -> F.TBits{ternary=true; len=sz}
  | C.TPat(_) -> err "TPat size should be a singleton"
  | C.TBits(Sz(sz)) -> F.TBits{ternary=false; len=sz}
  | C.TBits(_) -> err "TBits size should be a singleton"
and translate_acn_ty (aty : C.acn_ty) = 
  {
    F.arg_tys = List.map translate_ty aty.aarg_tys; 
    F.ret_ty = F.ttuple @@ List.map translate_ty aty.aret_tys;
    F.func_kind = FAction;
  }
and translate_ty (ty : C.ty) : F.ty = 
  {raw_ty = translate_raw_ty ty.raw_ty; 
   tspan = ty.tspan;
   (* tag "group" types because they are represented in FCore 
      as a non-unique Int type *)
   ty_ext = match ty.raw_ty with 
      | C.TGroup -> Some(F.TGroup) | _ -> None}
;;

(* let translate_decl (decl:C.decl) : F.decl = 
  match decl.d with 
  | C.DGlobal(id, ty, exp) -> 


;; *)
let translate_prog (ds : C.decls) : F.decls = 
  
  []
;;

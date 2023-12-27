open Batteries
open Syntax


(*  some type constructor helpers *)
let effectless_fun_rawty arg_tys ret_ty = 
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = start_eff
    ; constraints = ref []
    }
;;

let acn_rawty arg_tys ret_ty =
  TAction {
    aarg_tys = arg_tys;
    aret_tys = [ret_ty];
  }
;;

let tynum = ref (-1)
let fresh_rawty base_tyname = 
  incr tynum;
  let ty_id = Id.create (base_tyname ^ string_of_int !tynum) in
  (TQVar (QVar ty_id))
;;
let fresh_ty base_tyname =  
  ty_sp (fresh_rawty base_tyname) Span.default
;;

let fresh_tint () = 
  ty @@ TInt (IVar (QVar (Id.fresh "tint_sz")))
;;

let fresh_size base_id = 
  incr tynum;
  let size_id = Id.create ( base_id ^ string_of_int !tynum) in
   (IVar (QVar size_id))
;;



(* typing *)
let equiv_raw_tys raw_tys = 
  List.fold_left
    (fun (prev_res, prev_rty) rty  -> 
      let res = SyntaxUtils.equiv_raw_ty (TyTQVar.strip_links prev_rty) (TyTQVar.strip_links rty) in
      if (not res) then (
        print_endline ("type mismatch in actions. ");
      print_endline ("expected type: "^(Printing.raw_ty_to_string prev_rty));
      print_endline ("got type: "^(Printing.raw_ty_to_string rty));
      );
      (prev_res && res, prev_rty)) 
    (true, List.hd raw_tys)
    raw_tys
  |> fst
;;  


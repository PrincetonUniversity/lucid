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

open InterpSyntax
let taction iarg marg ret = 
  TActionConstr(
    {
      aconst_param_tys=[ty iarg];
      aacn_ty={
        aarg_tys=[ty marg];
        aret_tys=[ty ret];
      }
    })
;;
(* convert a function from ivals -> ivals to a function from values -> values *)
let ival_fcn_to_internal_action nst swid vaction = 
  let open CoreSyntax in
  let open InterpState in 
  let acn_cid, action_f = match vaction with 
    | F (Some(cid), f) -> cid, f
    | F (None, _) -> error "Table.install: interpreter error -- the action was added to the global context without a name"
    | _ -> error "Table.install: expected a function"
  in
  (* fill state and switch id args of the action function, 
     which don't matter because its a pure function  *)
  let acn_f  (vs : value list) : value list = 
    (* wrap vs in ivals, call action_f, unwrap results *)
    let ivals = List.map (fun v -> V v) vs in
    let result = action_f nst swid ivals in
    (* passing action and args separately to install makes the 
       action return a function *)
    let result = match result with 
      | F(_, f) -> 
        f nst swid []
      | V v ->
        V(v)
        (* extract_ival result *)
    in
    let result = extract_ival result in
    match result.v with
    | VTuple(vs) -> List.map value vs
    | _ -> [result]
  in
  acn_cid, acn_f
;;

let rec flatten_v (v : CoreSyntax.v) = 
  match v with 
  | VTuple(vs) -> 
    (List.map flatten_v vs) |> List.flatten
  | VRecord(id_vs) -> 
    List.split id_vs |> snd |> List.map flatten_v |> List.flatten
  | VBool _ | VInt _ | VEvent _ | VGlobal _ 
  | VPat _  | VGroup _ | VBits _ -> [v]
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


(* let bitmatch bits n =
  let open Z in
  let bits = List.rev bits in
  let two = Z.of_int 2 in
  let rec aux bits n =
    match bits with
    | [] -> n = zero
    | hd :: tl ->
      aux tl (shift_right n 1)
      &&
      (match hd with
       | 0 -> rem n two = zero
       | 1 -> rem n two <> zero
       | _ -> true)
  in
  aux bits n
;; *)

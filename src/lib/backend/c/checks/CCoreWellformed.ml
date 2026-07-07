(* misc form checks at stages in the CCore compiler. *)

open CCoreSyntax
open CCoreExceptions

let error msg = Console.show_message msg ANSITerminal.Red "error"; raise (TypeError "")
;;
let err_in_decl decl_opt str = 
  match decl_opt with
  | None -> 
    error str
  | Some decl -> error @@ 
      str^"\n"
      ^(Printf.sprintf 
        "\n------ in declaration ----- \n%s\n----------------------\n" 
        (CCorePPrint.decl_to_string decl))



(* check for mask operations on non-container-width operations *)
let is_maskable_result ty =
  match ty.raw_ty with TInt n -> CCoreMaskWidths.maskable n | _ -> false

let mask_invariant_checker = object
  inherit [_] s_iter as super
  val mutable cur_decl_opt = None
  val mutable masked = []   (* physical identities of masked sub-expressions *)
  method! visit_decl () decl =
    cur_decl_opt <- Some decl;
    super#visit_decl () decl
  method! visit_exp () exp =
    (match exp.e with
     | EOp(BitAnd, [inner; { e = EVal _; _ }]) -> masked <- inner :: masked
     | EOp(op, _)
       when CCoreMaskWidths.is_violating_op op
            && is_maskable_result exp.ety
            && not (List.memq exp masked) ->
       err_in_decl cur_decl_opt @@ Printf.sprintf
         "non-standard-width value is not masked (CCoreMaskWidths should have wrapped it): >>> %s <<<"
         (CCorePPrint.exp_to_string exp)
     | _ -> ());
    super#visit_exp () exp
end
;;

let tuple_assign_checker = object 
  inherit [_] s_iter as super 
  val mutable cur_decl_opt = None
  method! visit_decl () decl = 
    cur_decl_opt <- Some decl;
    super#visit_decl () decl

  method! visit_OTupleLocal _ _ _ = 
    err_in_decl cur_decl_opt "tuple locals are not supported in C"
  method! visit_OTupleAssign _ _ = 
    err_in_decl cur_decl_opt "tuple locals are not supported in C"
end


(* A field may only cross a byte boundary if its byte-aligned at its start or end. *)
let byte_layout_ok (widths : int list) : bool =
  let rec go off = function
    | [] -> off mod 8 = 0
    | n :: rest ->
      let bit_offset_in_byte = off mod 8 in
      let straddles = bit_offset_in_byte + n > 8 in
      let aligned = bit_offset_in_byte = 0 || (off + n) mod 8 = 0 in
      if straddles && not aligned then false
      else go (off + n) rest
  in
  go 0 widths
;;
let check_event_params decls =
  match CCoreVariants.find_variant_sigs decls with
  | None -> ()
  | Some sigs ->
    List.iter
      (fun s ->
        let arm = CCoreVariants.arm_of_sig s in
        let ename = CCorePPrint.cid_to_string arm.ctor in
        (* every field must be a scalar int/bool *)
        List.iter
          (fun (fid, fty) ->
            match (base_type fty).raw_ty with
            | TInt _ | TBool -> ()
            | _ ->
              error @@ Printf.sprintf
                "event %s: field %s has an aggregate type (%s), not supported by the C backend yet"
                ename (CCorePPrint.cid_to_string fid) (CCorePPrint.ty_to_string fty))
          arm.params;
        let widths = List.map (fun (_, fty) -> sizeof_ty fty) arm.params in
        if not (byte_layout_ok widths) then
          error @@ Printf.sprintf
            "event %s: invalid field byte layout (fields must total whole bytes, and any field crossing a byte boundary must start or end on one)"
            ename)
      sigs
;;

let op_checker = object
  inherit [_] s_iter as super
  val mutable cur_decl_opt = None
  method! visit_decl () decl = cur_decl_opt <- Some decl; super#visit_decl () decl
  method! visit_exp () exp =
    super#visit_exp () exp;
    match exp.e with
    | EOp(Conc, _) ->
      err_in_decl cur_decl_opt "concatenation (Lucid `^`) is not yet supported in the C backend"
    | EOp((SatPlus | SatSub), _) ->
      err_in_decl cur_decl_opt "saturating arithmetic (`|+|` / `|-|`) is not yet supported in the C backend"
    | _ when is_egen_group exp ->
      err_in_decl cur_decl_opt "generate_ports (multicast) is not supported by the C backend"
    | _ -> ()
end


(* Check that pointers are not used (for before lowering to C) *)
let ptr_checker =
  object (_)
    inherit [_] s_iter as super

    val mutable cur_decl = None
    
    method! visit_decl env decl =
      match decl.d with
      (* skip ffuns *)
      | DForiegn _ -> ()
      | DFun(_, _, _, _, BForiegn _) -> ()
      | _ ->
        cur_decl <- Some decl;
        super#visit_decl env decl

    method! visit_TPtr _ =
      err_in_decl cur_decl "pointer types are not supported in the C backend";

    method! visit_EDeref _ =
      err_in_decl cur_decl "dereference operator (*) is not supported in the C backend"
  end
;;

let check_ccore_compat decls =
  check_event_params decls;
  List.iter (fun d -> op_checker#visit_decl () d) decls
;;

let check_no_ptrs decls = ptr_checker#visit_decls () decls


let check_c_compat decls =
  List.iter (fun decl ->
    mask_invariant_checker#visit_decl () decl;
    tuple_assign_checker#visit_decl () decl;

  )
  decls
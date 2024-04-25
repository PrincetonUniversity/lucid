(* misc checks to make sure constructs not supported in 
   C have been eliminated.  *)

open CCoreSyntax
open CCoreExceptions


(*** Bit ints ***)
(* Bit ints (ints that are not 8, 16, 32, or 64 bits) have these rules: 
  1. variables may not be declared as a bit int type
  2. functions and events may not have bit int parameters 

  These restrictions let us implement bit ints as bit fields in c, 
  with the addition of printing cast(int<b>) ops as modulo operations.

  Additionally: 
    3. records must be byte sizes (no 7-bit, 19-bit, 81-bit, etc, records)

  This makes sure that the builtin parser functions can operate directly 
  on a byte stream rather than a bit stream.
*)

let is_tbit_int ty = 
  match ty.raw_ty with 
  | TInt size ->( 
    match size with 
    | 8 | 16 | 32 | 64 -> false
    | _ -> true)
  | _ -> false
;;

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

let bitint_checker = object 
  inherit [_] s_iter as super 
  val mutable cur_decl_opt = None
  method! visit_decl () decl = 
    cur_decl_opt <- Some decl;
    super#visit_decl () decl

  method! visit_OLocal () cid ty = 
    if (is_tbit_int ty) then 
      err_in_decl cur_decl_opt  @@
        Printf.sprintf "int variables must be 8, 16, 32, or 64 bytes. Wrap it in a record padded to an even byte width. >>> %s %s <<<"
          (CCorePPrint.ty_to_string ty) 
          (CCorePPrint.cid_to_string cid)    

  method! visit_params () params = 
    List.iter (fun (id, ty) -> 
      if (is_tbit_int ty) then 
        err_in_decl cur_decl_opt@@      
        Printf.sprintf 
          "int parameters must be 8, 16, 32, or 64 bytes. Wrap it in a record padded to an even byte width. >>> %s %s <<<" 
          (CCorePPrint.ty_to_string ty) 
          (CCorePPrint.cid_to_string id)) 
    params
  method! visit_ty () ty = 
    super#visit_ty () ty;
    let ty = base_type ty in
    match ty.raw_ty with 
    | TRecord(_, tys) 
    | TUnion(_, tys) 
    | TTuple tys -> 
      let bit_sizes = List.map bitsizeof_ty tys in
      let total_size = List.fold_left (+) 0 (List.map (fun sz -> match sz with | Some sz -> sz | None -> 0) bit_sizes) in
      if (total_size mod 8 <> 0) then 
        err_in_decl cur_decl_opt @@"record, union, or tuple >>>"^(CCorePPrint.ty_to_string ty)^" <<< has a total size that is not a multiple of 8 bits ("^(string_of_int total_size)^" bits)"
    | _ -> ()
end
;;


(* make sure that all OTupleLocal and OTupleAssign ops have been eliminated *)
(* the only time these constructs appear is in Table.lookup return values, 
   when the frontend eliminates records and user-defined tuples. *)
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

(* make sure there are no TBit-typed values. 
   These should be removed from lucid entirely. 
   They were just added for ternary tables, but we can just as well 
   do it with a (value, mask) pair. *)
let tbit_checker = object 
  inherit [_] s_iter as super 
  val mutable cur_decl_opt = None
  method! visit_decl () decl = 
    cur_decl_opt <- Some decl;
    super#visit_decl () decl

  method! visit_ty () ty = 
    super#visit_ty () ty;
    match ty.raw_ty with 
    | TBits _ -> 
      err_in_decl cur_decl_opt "TBit types are not supported in C"
    | _ -> ()
end




let all_checks decls = 
  List.iter (fun decl -> 
    bitint_checker#visit_decl () decl;
    tuple_assign_checker#visit_decl () decl;
    tbit_checker#visit_decl () decl;
    
  ) 
  decls
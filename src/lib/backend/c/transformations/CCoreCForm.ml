(* a few small passes to get the program into a form 
   that is compatible with C.
   - convert match statements to if statements
   - pull record and union expressions out of 
     non-local-declaration statements     
*)
open CCoreSyntax
open CCoreExceptions

(* Convert match statements to if statements *)
let bitstring_to_maskedint (bs : int list) : int*int = 
  let to_val_and_mask bit = 
    match bit with 
    | 0    -> (0, 1) (* val: 0, mask 1 *) 
    | 1    -> (1, 1) (* val: 1, mask 1 *) 
    | -1  -> (0, 0) (* val: 0, mask :0 *)
    | _ -> err "invalid bitstring bit"
  in
  let rec bitstring_to_int bits =  
    match bits with 
      | [] -> 0
      | hd::tl ->
        (Int.shift_left hd (List.length tl)) + (bitstring_to_int tl)
  in      
  let vbits, mbits = List.map to_val_and_mask bs |> List.split in 
  bitstring_to_int vbits, bitstring_to_int mbits
;;

let to_bool_atom (e,p) = 
  match p with 
    | PVal {v=VBits{ternary=true; bits;};} -> 
        let value, mask = bitstring_to_maskedint bits in
        let value = vint value (List.length bits) in
        let mask = vint mask (List.length bits) in
        let rhs = eop BitAnd [eval value; eval mask] in
        let lhs = eop BitAnd [e; eval mask] in
        Some(eop Eq [lhs; rhs])
    | PVal v -> Some(eop Eq [e; eval v])
    | PWild _ -> None (* no constraint *)
    | PEvent _ -> err "events must be eliminated before matches"
;;
let eand_list terms = 
  match terms with 
  | [] -> vbool true |> eval
  | [term] -> term
  | terms -> 
      List.fold_left 
        (fun acc term -> eop And [acc; term])
        (List.hd terms)
        (List.tl terms)
;;
let rec match_to_if exps branches = 
  match branches with 
  | [] -> snoop
  | [pats, stmt] -> 
    let econd = eand_list 
      (List.filter_map to_bool_atom (List.combine exps pats))
    in
    sif econd stmt snoop
  | (pats, stmt)::branches -> 
    let else_stmt = match_to_if exps branches in
    let econd = eand_list 
      (List.filter_map to_bool_atom (List.combine exps pats))
    in
    sif econd stmt else_stmt
;;
let transform_match stmt = 
  match stmt.s with 
  (* a match on a single integer is just a switch *)
  | SMatch([exp], _) when is_tint exp.ety -> stmt
  | SMatch([exp], _) when is_tenum exp.ety -> stmt
  | SMatch(es, branches) -> match_to_if es branches
  | _ -> stmt
;;
let normalize_matches decls = 
  CCoreTransformers.subst_statement#visit_decls transform_match decls
;;

(* ensure that record, union, and tuple expressions only appear in declarations *)
let is_initializer exp = match exp.e with 
  | EUnion _ -> true 
  | ERecord _ -> true
  | ETuple _ -> true
  | _ -> false
;;
let normalize_struct_inits decls = 
  let v = object 
    (* new statements that must be added before the current one *)
    val mutable new_stmts = []
    inherit [_] s_map as super
    
    method! visit_statement () stmt = 
      match stmt.s with 
      (* creating a new local -- nothing has to change, so don't recurse *)
      | SAssign(OLocal _, _) -> stmt 
      (* pull initializers out of other statements *)
      | _ -> (
        new_stmts <- [];
        let stmt = super#visit_statement () stmt in
          match new_stmts with 
          | [] -> stmt
          | _ -> stmts (new_stmts@[stmt])
      )
    method! visit_exp () exp = 
      if (is_initializer exp) 
      then 
        let id = Id.fresh_name "tmp" in
        let stmt = slocal (Cid.id id) exp.ety exp in
        new_stmts <- new_stmts@[stmt];
        evar (Cid.id id) exp.ety
      else exp
    end
  in
  v#visit_decls () decls
;;
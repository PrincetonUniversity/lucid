(* 
  This pass deduplicates hash and Array accessors, by 
  moving them all into their own labeled blocks. 
  Similar to ActionForm pass, but applied to the 
  bodies of the labeled statements created in that 
  pass, rather than the main statement.
*) 

open CoreSyntax
open TofinoCore
open InterpHelpers
open ActionForm

exception Error of string
let error s = raise (Error s)

let dedup_stmt prev_cache stmt =  
  let new_decls = ref [] in
  let add_decl_opt d_opt = match d_opt with 
    | Some(d) -> new_decls := d::(!new_decls)
    | _ -> ()
  in

  let blockcache = ref prev_cache in 
  let v = object
    inherit [_] s_map as super

    method! visit_statement ctx stmt =
      let stmt = super#visit_statement ctx stmt in
      match stmt.s with 
      | SUnit(exp)
      | SAssign(_, exp)
      | SLocal(_, _, exp) -> (
        match exp.e with
        | ECall(_) | EHash(_) -> (
          let new_cache, call_block, new_block_opt = 
            dedup_branch (!blockcache) stmt
          in
          add_decl_opt new_block_opt;
          blockcache := new_cache;
          call_block
        )
        | _ -> (
          let new_cache, call_block, new_block_opt = 
            dedup_branch (!blockcache) stmt
          in
          add_decl_opt new_block_opt;
          blockcache := new_cache;
          call_block
          (* stmt *)
        )
      )
      | _ -> stmt
    end
  in
  let new_stmt = v#visit_statement () stmt in
  !new_decls, new_stmt, (!blockcache)
;;


let rec dedup_decls cache tds =
  match tds with 
  | [] -> tds
  | tdecl::tds -> (
    match tdecl.td with
    | TDLabeledBlock(id, stmt) -> 
      let new_decls, new_stmt, new_cache = dedup_stmt cache stmt in      
      let tdecl = {tdecl with td=TDLabeledBlock(id, new_stmt)} in
      new_decls@[tdecl]@(dedup_decls new_cache tds)
    | _ -> tdecl::(dedup_decls cache tds)
  )

let rec process tds = 
  let tds = dedup_decls empty_blockcache tds in
  tds 
;;

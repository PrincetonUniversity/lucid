(* 
  This pass deduplicates hash and Array accessors, by 
  moving them all into their own labeled blocks. 
  This applies only to labeled statements, not handlers. 
  Must be used after ActionForm.ml.
  Note: This re-uses the labeled_statement cache and process_branch 
  code from ActionForm.ml.
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
    | Some(d) -> new_decls := (!new_decls)@[d]
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
            process_branch (!blockcache) stmt
          in
          add_decl_opt new_block_opt;
          blockcache := new_cache;
          call_block
        )
        | _ -> (
          let new_cache, call_block, new_block_opt = 
            process_branch (!blockcache) stmt
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
  (* dedup closures that represent tables *)
  match tds with 
  | [] -> tds
  | tdecl::tds -> (
    match tdecl.td with
    | TDOpenFunction(id, [], stmt) -> 
      let new_decls, new_stmt, new_cache = dedup_stmt cache stmt in      
      let tdecl = {tdecl with td=TDOpenFunction(id, [], new_stmt)} in
      new_decls@[tdecl]@(dedup_decls new_cache tds)
    | _ -> tdecl::(dedup_decls cache tds)
  )

let rec process tds = 
  let tds = dedup_decls empty_labeled_statement_cache tds in
  tds 
;;

let process_comp comp =
  {comp with comp_decls = process comp.comp_decls}
;;

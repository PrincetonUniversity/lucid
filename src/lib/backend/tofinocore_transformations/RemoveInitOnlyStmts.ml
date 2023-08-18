(* remove statements tagged with the PNoInitLocal pragma 
   and add the variable id to the handler's internal 
   variables list. *)

open CoreSyntax
open TofinoCoreNew


let eliminator = object (self)
  inherit [_] s_map as super
  method declared_locals = ref []
  method! visit_statement _ stmt  = 
    let stmt = super#visit_statement () stmt in
    match (stmt.s, Pragma.exists_pnolocal stmt.spragmas) with 
    | (SLocal(id,ty, _), true) -> 
      self#declared_locals := (id, ty) :: !(self#declared_locals);
      snoop
    | _ -> stmt
  end
;;

let process (core_prog : prog) = 
  Base.List.map core_prog
    ~f:(fun component -> 
      let comp_decls' = Base.List.map component.comp_decls
        ~f:(fun decl -> match decl.td with
            | TDHandler(HEvent(h)) -> (
              (* we're collecting the new declared variables in this handler *)
              let new_declared_locals = ref [] in
              let hdl_body' = 
                match h.hdl_body with
                | SFlat(stmt) -> 
                  let stmt' = eliminator#visit_statement () stmt in
                  let locals = !(eliminator#declared_locals) in
                  new_declared_locals := (!new_declared_locals) @ locals;
                  SFlat(stmt')
                | SPipeline(stmts) ->
                  let stmts' = List.map (fun stmt -> 
                      let stmt' = eliminator#visit_statement () stmt in
                      let locals = !(eliminator#declared_locals) in
                      new_declared_locals := (!new_declared_locals) @ locals;
                      stmt'
                    ) stmts 
                  in
                  SPipeline(stmts')
              in
              let h' = HEvent({h with 
                hdl_body = hdl_body'; 
                hdl_preallocated_vars = h.hdl_preallocated_vars@(!new_declared_locals)}) 
              in
              let td' = TDHandler(h') in
              { decl with td = td'; }
            )
            (* non-handler declaration -- do nothing *)
            | _ -> decl
        )
    in
    let component' = { component with comp_decls = comp_decls';} in 
    component'
    )

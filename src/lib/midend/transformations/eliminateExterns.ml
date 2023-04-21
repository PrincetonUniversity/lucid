(* Delete extern calls *)
open CoreSyntax
open Batteries
module CL = Caml.List


let eliminate_externs ds = 
  let is_extern decl = 
    match decl.d with 
      | DExtern(id, _) -> Some(Cid.id id)
      | _ -> None
  in 
  let extern_cids = 
    CL.filter_map is_extern ds
  in 
  let v = 
    object
      inherit [_] s_map as super

      method !visit_statement ctx stmt = 
        match stmt.s with 
          | SUnit(exp) -> (
            match exp.e with 
              | ECall(cid, _) -> (
                if (CL.exists (Cid.equals cid) extern_cids)
                then ({stmt with s=SNoop})
                else (stmt)
              )
              | _-> stmt
          )
          | _ -> super#visit_statement ctx stmt

      method !visit_complex_body _ bdy = 
        {bdy with extern_calls=[]}

    end
  in
  v#visit_decls () ds
;; 




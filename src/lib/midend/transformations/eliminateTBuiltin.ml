(* convert TBuiltin table types to TName, which is still used 
   in the current tofino and interp implementations *)
open CoreSyntax

let v =
   object (self)
     inherit [_] s_map as super
     method! visit_TBuiltin ctx cid raw_tys = 
      if (Cid.equals cid Tables.t_id) then (
         let raw_tys = List.map (self#visit_raw_ty ctx) raw_tys in
         let key_raw_ty = List.nth raw_tys 0 in
         let install_raw_ty = List.nth raw_tys 1 in
         let param_raw_ty = List.nth raw_tys 2 in
         let ret_raw_ty = List.nth raw_tys 3 in
         let rec flatten (raw_ty : raw_ty) : raw_ty list = 
            match raw_ty with 
               | TTuple(tys) -> 
                  List.map flatten tys |> List.concat
               | _ -> [raw_ty]
         in
         let key_raw_tys = flatten key_raw_ty in
         let install_raw_tys = flatten install_raw_ty in
         let param_raw_tys = flatten param_raw_ty in
         let ret_raw_tys = flatten ret_raw_ty in
      
         (* err_unsupported tspan "TBuiltin IR translation not implemented" *)
         let rawty_to_intsize (raw_ty) = 
         match raw_ty with 
         | TInt(Sz sz) -> sz
         | TBool ->1
         | _ -> 
            error "[rty_to_size] expected an integer, but got something else"
         in
      
         let tkey_sizes = Szs (List.map rawty_to_intsize key_raw_tys) in
         let tinstall_sizes = Szs (List.map rawty_to_intsize install_raw_tys) in
         let tparam_sizes = Szs (List.map rawty_to_intsize param_raw_tys) in
         let tret_sizes = Szs (List.map rawty_to_intsize ret_raw_tys) in
         TName(Tables.t_id, [tkey_sizes; tinstall_sizes; tparam_sizes; tret_sizes])         
      )
      else 
         super#visit_TBuiltin ctx cid raw_tys
   end



let process_prog ds = 
   v#visit_decls () ds
;;
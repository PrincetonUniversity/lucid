(* 
   Some small passes to adjust the tofinocore backend 
   for the new TBuiltin types. 

   1.wrap calls to TBuiltin functions that do not return tuples 
      in a TupleAssign statement. The tofino backend expects
      Table.lookup calls to only ever appear inside of
      STupleAssign statements. But there is no reason for the 
      frontend to enforce this, and no reason for other 
      backends to expect it. So we just wrap the calls here
      in a one-off pass until we can adjust the tofino backend 
      to not rely on this convention. 

   2.convert TBuiltin table types to TName, which is still used 
      in the current tofino and interp implementations. *)
open CoreSyntax


let tbuiltin_transformer =
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



(* we don't want _ALL_ the builtin cids, just the ones that 
   need to be wrapped in a tupleassign, i.e., Tables *)
let builtin_cids = 
List.map
   (fun (_, _, global_funs, constructors) ->
      let fun_cids =
      List.map
         (fun (gf : InterpState.global_fun) -> gf.cid)
         global_funs
      in
      let constructor_cids = List.map fst constructors in
      fun_cids @ constructor_cids)
   (List.map LibraryInterface.sigty_to_tup [Tables.signature])
|> List.flatten
;;
      
let is_tuple ty =
   match ty.raw_ty with
   | TTuple _ -> true
   | _ -> false
 ;;
 
let stupleassign_wrapper = 
   object 
      inherit [_] s_map as super
      method! visit_statement _ s = 
         match s.s with
         | SAssign (id, { e = ECall(fcncid, args, u); ety; espan}) 
         when ((List.mem fcncid builtin_cids) && not (ety |> is_tuple)) ->
            (* let args_pre_stmt, args' = eliminate_exps args in *)
            (* let exp' = { e = ECall (cid, args', u); ety; espan } in *)
            let stupleassign = statement@@STupleAssign { ids = [Cid.to_id id]; tys = None; exp = { e = ECall(fcncid, args, u); ety; espan} } in
            stupleassign
         | SLocal (id, ty, { e = ECall(fcncid, args, u); ety; espan}) 
         when ((List.mem fcncid builtin_cids) && not (ety |> is_tuple)) ->
            let stupleassign = statement@@STupleAssign { ids = [id]; tys = Some([ty]); exp={ e = ECall(fcncid, args, u); ety; espan} } in
            stupleassign
         (* sseq args_pre_stmt stupleassign *)
         | _ -> super#visit_statement () s
   end
;;
   

let process_prog ds = 
   tbuiltin_transformer#visit_decls () ds |>
   stupleassign_wrapper#visit_decls ()
;;
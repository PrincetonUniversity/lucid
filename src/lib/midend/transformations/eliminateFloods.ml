(* pull floods out of the generate statements, then 
   eliminate them using a match statement. *)
open CoreSyntax
open Batteries
open InterpHelpers
open MiscUtils
(* TODO: should be part of configuration. *)
let active_ports = ref [128; 129; 130; 131];; 
let flood_ports_of p = 
  list_sub !active_ports [p]
;;

let set_active_ports ports = 
  active_ports := ports 
;;

(* pull floods out of generate statements 
  generate_ports(flood(foo), ev());
  --> 
  group tmp = flood(foo);
  generate_ports(tmp, ev()); *)
let eliminate_floods ds = 
  let extract_floods = 
    object
      inherit [_] s_map as super
      method !visit_statement ctx statement = 
        match statement.s with 
        | SGen(GMulti(gexp), evexp) -> (
          match gexp.e with 
          | EFlood(_) -> (
            let pc_var = Id.fresh "flood_pc" in 
            (* new statement to precompute *)
            let pc_stmt = slocal 
              pc_var
              (ty TGroup)
              (gexp)
            in 
            let pc_evar = var_sp
              (Cid.id pc_var)
              (ty TGroup)
              Span.default
            in 
            sseq
              pc_stmt
              {statement with s=SGen(GMulti(pc_evar), evexp)}
          )
          (* not a flood, nothing to transform. *)
          | _ -> statement
        )
        (* recurse on all other statements. *)
        | _ -> super#visit_statement ctx statement
    end
  in

  (* replace assignments or locals with flood expressions 
     in the rhs with a sequence: 
     group foo = flood x; 
     --> 
     group foo = {0};
     match x with 
       | 0 -> foo = {1, 2, 3, 4, ...};
       | 1 -> foo = {0, 1, 2, 3, ...};
       | 2 -> foo = {0, 1, 3, 4, ...};
       ...

   *)
  let eliminate_floods = 
    object
      inherit [_] s_map as super
      method !visit_statement ctx statement = 
        (* create a branch:
            | port -> groupvar = {activeports - port}; 
        *)
        let flood_branch groupvar port : branch = 
          let groupexp = value_to_exp 
            (vgroup (flood_ports_of port))
          in     
          ([PNum (Z.of_int port)], sassign groupvar groupexp)
        in 
        (* create a match on floodarg that sets groupvar to the appropriate value. *)
        let flood_match groupvar floodarg = 
          smatch 
            [floodarg] 
            (List.map (flood_branch groupvar) !active_ports)      
        in         
        match statement.s with 
        | SAssign(groupvar, floodexp) -> (
          match floodexp.e with 
          | EFlood(floodarg) -> 
            flood_match (groupvar) floodarg
          | _ -> statement
        )
        | SLocal(groupvar, groupty, floodexp) -> (
          match floodexp.e with 
          | EFlood(floodarg) -> 
            sseq 
              (* initialize to all active ports *)
              (slocal groupvar groupty (vgroup !active_ports |> value_to_exp)) 
              (flood_match (Cid.id groupvar) floodarg)
          | _ -> statement
        )
        | _ -> super#visit_statement ctx statement
    end

  in 
  (* *)
  extract_floods#visit_decls () ds |> eliminate_floods#visit_decls ()
;; 




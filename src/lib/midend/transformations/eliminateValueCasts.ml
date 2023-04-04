(* convert casts of values into values. 

  (int<<2>>)1 --> 1w2

*)
open CoreSyntax
open Batteries


(* delete the assignment outer := tgt *)
(* let delete_assignment ds outer_id ret_id = 
  let v = 
    object
      inherit [_] s_map as super
      method !visit_statement _ st = 
(*         print_endline("[delete_assignment] on:");
        print_endline (CorePrinting.statement_to_string st); *)
        let s = match st.s with 
          | SAssign(id, {e=EVar(cid)}) -> (
            (* print_endline ("match SAssign"); *)
            if ((Id.equals id (outer_id)) & (Id.equals (Cid.to_id cid) ret_id))
            then (SNoop)
            else (st.s)
          )
          | _ -> st.s
        in 
        super#visit_statement () {st with s=s}        
    end
  in 
  v#visit_decls () ds
;;
 *)

let eliminate_value_casts ds = 
  let v = 
    object
      inherit [_] s_map as super

      method !visit_e ctx e = 
        let e = super#visit_e ctx e in 
        let _ = ctx in 
        match e with 
          | EOp(Cast(sz), [{e=EVal({v=VInt(i); _}); _}]) -> (   
            EVal(vinteger (Integer.set_size sz i))
          )
  (* | Cast size, [v] -> vinteger (Integer.set_size size (raw_integer v)) *)

            (* EVal(v) *)
          | _ -> e
    end
  in
  v#visit_decls () ds
;; 




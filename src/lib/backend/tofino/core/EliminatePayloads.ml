(* Payloads are implicit in the tofino compiler. There are no Payload variables, 
   and you can't create empty payloads. We can implement empty payloads, 
   down the line, by using mirroring. *)

open CoreSyntax 
open Core

let warn str = 
  Console.show_message str ANSITerminal.Yellow "Unsupported feature"
;;


let pre_check ds = 
  let v = 
    object
    inherit [_] s_iter as super
    method! visit_exp _ exp = 
      match exp.e with
      | ECall(cid, _) when (Cid.equal cid Payloads.payload_empty_cid) -> 
        CoreSyntax.error ("The Payload.empty() function is not supported on the tofino.")
      | _ -> super#visit_exp () exp
    end
  in
  v#visit_decls () ds
;;
   
let rec process ds =   
  let v = 
    object
      inherit [_] s_map as super
      method! visit_params ctx params = 
        let params = super#visit_params ctx params in
        (* delete any parameters with payload type *)
        let params = List.filter params ~f:(fun (_, ty) -> 
          not (InterpPayload.is_payload_ty ty))
        in
        params
      method! visit_exp ctx exp = 
        match exp.e with
        | ECall(cid, args) ->
          (* delete any args with payload type *)
          let args = List.filter args ~f:(fun arg -> 
            not (InterpPayload.is_payload_ty arg.ety)) in
          { exp with e = ECall(cid, args) }
        | _ -> super#visit_exp ctx exp
    end
  in
  v#visit_decls () ds
;;

let post_check ds =
  let v = 
    object
    inherit [_] s_iter as super
    method! visit_ty _ ty = 
      if (InterpPayload.is_payload_ty ty) then
        CoreSyntax.error ("Payload type expressions should have been eliminated")
      else
        super#visit_ty () ty
    method! visit_exp _ exp = 
      if (InterpPayload.is_payload_ty exp.ety) then
        CoreSyntax.error ("Payload type expressions should have been eliminated"^(CorePrinting.exp_to_string exp)) 
      else
        super#visit_exp () exp
    end
  in
  v#visit_decls () ds
;;
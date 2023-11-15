(* Payloads are implicit in the tofino compiler -- there are no payload values. 
  Before this pass, payloads can appear in: 

  - parser arguments
  - parser calls
  - event declarations
  - event constructor calls
  - handler declarations

  This pass deletes payloads from: 
    - event declarations
    - event constructor calls
    - handler declarations

  It leaves payloads in parser arguments and calls, because those do appear in P4. 

*)

open CoreSyntax 
open Core

let warn str = 
  Console.show_message str ANSITerminal.Yellow "Unsupported feature"
;;

(* a map from event id to has_payload, which indicates that the last argument of the event is a payload *)
let empty_ctx = Collections.CidSet.empty;;
type ctx = Collections.CidSet.t;;

let filter_payload_params params = 
  List.filter params ~f:(fun (_, ty) -> 
    not (InterpPayload.is_payload_ty ty))
;;

(* in a statement, delete the payload argument (the last arg) in calls to constructors of events with payloads *)
let process_inner = 
    object
      inherit [_] s_map as super

      method! visit_exp events_with_payloads exp = 
        match exp.e with 
        | ECall(cid, args, u) when (Collections.CidSet.mem (cid) events_with_payloads) -> 
          (* payload is always the last argument *)
          let args = Caml.List.rev args |> Caml.List.tl |> Caml.List.rev in
          { exp with e = ECall(cid, args, u) }
        | _ -> 
          (* super means revisit exp with default method, which will skip exp itself and recurse on subnodes *)
          super#visit_exp events_with_payloads exp
    end
;;

let process_decl (events_with_payloads: ctx) (d:decl) = 
  match d.d with 
  | DEvent(id, num_opt, esort, params) -> (
    let rev_params = List.rev params in
    let fst_param = List.hd rev_params in 
    match fst_param with 
    (* if the first parameter is a payload, delete it and update context *)
    | Some(_, ty) when (InterpPayload.is_payload_ty ty) -> 
      let events_with_payloads = Collections.CidSet.add (Cid.id id) events_with_payloads in
      let params = Caml.List.tl rev_params |> List.rev in
      events_with_payloads, { d with d = DEvent(id, num_opt, esort, params) }
    | _ -> events_with_payloads, d
  )
  | DHandler(id, hdl_sort, (params, stmt)) -> 
    let params = filter_payload_params params in
    let stmt = process_inner#visit_statement events_with_payloads stmt in
    events_with_payloads, { d with d = DHandler(id, hdl_sort, (params, stmt)) }
  | DParser(id, params, pblock) -> 
    let pblock = process_inner#visit_parser_block events_with_payloads pblock in
    events_with_payloads, { d with d = DParser(id, params, pblock) }
  |  _ -> 
    events_with_payloads, d
;;
let rec process ctx ds = 
  match ds with 
  | [] ->ctx, []
  | d :: ds -> 
    let ctx, d = process_decl ctx d in
    let ctx, ds = process ctx ds in
    ctx, (d::ds)
;;

let process ds = 
  let _, ds = process empty_ctx ds in
  ds
;;

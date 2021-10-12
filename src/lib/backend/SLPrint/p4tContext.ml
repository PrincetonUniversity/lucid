(* context for p4t printing, derived entirely from tofino ir. *)
open LLSyntax
open Batteries
module CL = Caml.List

module CtxTbl = struct
  type t = Cid.t

  let compare = Cid.compare
end

module P4tCtx = BatMap.Make (CtxTbl)

let p4tCtx : decl P4tCtx.t ref = ref P4tCtx.empty

let ctx_add_structdef decl =
  match decl with
  | StructDef _ -> p4tCtx := P4tCtx.add (name_of_structdef decl) decl !p4tCtx
  (* also add const defs, for the final emit table. *)
  | DConst (var_id, _, _, _) ->
    print_endline
      ("[ctx_add_structdef] adding const def: " ^ Cid.to_string var_id);
    p4tCtx := P4tCtx.add var_id decl !p4tCtx
  | _ -> ()
;;

let ctx_add_structdefs decls = CL.iter ctx_add_structdef decls
let ctx_get_structdef structname = P4tCtx.find structname !p4tCtx
let ctx_get_structdef_ty cid = ty_of_structdef (ctx_get_structdef cid)

(* Temporary hack. Need to restructure IR, or something, to 
avoid duplicating this code from LLOp. *)
let defname_from_evcid evcid =
  print_endline ("[defname_from_evcid]: " ^ Cid.to_string evcid);
  (*   match (Cid.to_ids evcid) with 
    | [] *)
  Cid.id ("e_" ^ fst (Cid.to_id evcid), snd (Cid.to_id evcid))
;;

let ctx_get_event_iid event_id =
  let event_enum_var_id = defname_from_evcid event_id in
  match P4tCtx.find event_enum_var_id !p4tCtx with
  | DConst (_, _, iid, _) -> iid
  | _ ->
    error
      "[ctx_get_event_iid] unexpected declaration when looking for event enum."
;;

let ctx_add_structinst decl =
  match decl with
  | StructVar _ -> p4tCtx := P4tCtx.add (name_of_structinst decl) decl !p4tCtx
  | _ -> ()
;;

let ctx_add_structinsts decls = CL.iter ctx_add_structinst decls

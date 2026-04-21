open Batteries
open Syntax
open SyntaxUtils
open Collections

(* This pass converts events/handlers with polymorphic parameters into 
   multiple monomorphic events/handlers, one for each unique type signature
   of parameters used in the program. *)


(* event id -> event arg types *)
module IdMap = Collections.IdMap

(* The template for a concrete instance of a poly event *)
type concrete_sig = {id : id; concrete_tys : ty list;}

(* everything about an event and handler declaration *)
type event_decl = {
  id:id; 
  params : params;
  ecalls : concrete_sig list; (* calls seen so far *)
}
type event_map = event_decl IdMap.t


(* Add an event call to the event declaration, if one with that 
    type signature doesn't already exist.  *)
let add_concrete_sig event_decl call_args : (id * event_decl) = 
  (* first, check to see if a call with the type signature exists *)
  let arg_tys = List.map (fun exp -> Option.get exp.ety) call_args in
  let fst_matching_call_opt = List.find_opt 
    (fun (call : concrete_sig) -> equiv_lists 
      (equiv_ty ~ignore_effects:true) 
      arg_tys 
      call.concrete_tys)
    event_decl.ecalls
  in
  match fst_matching_call_opt with
  | Some call -> (call.id, event_decl) (* if it does, return the existing monomorphic id *)
  | None -> (* if it doesn't, create a new monomorphic id and add the call to the event declaration *)
    let id' = Id.create ((Id.name (event_decl.id)) ^ "_" ^ string_of_int (List.length event_decl.ecalls)) in
    let exp_to_rawty exp = (Option.get exp.ety) in
    let concrete_tys = List.map exp_to_rawty call_args in 
    let ecalls' = event_decl.ecalls@[{id=id'; concrete_tys}] in
    id', {event_decl with ecalls = ecalls'}
;;

(* New main function *)
let update_calls ds : event_decl IdMap.t * decls = 
  (* use an object to visit DEvents and find all event decls *)
  let obj = object (self)
    inherit [_] s_map as super
    val mutable event_map = IdMap.empty
    method event_map = event_map
    (* entry point *)
    method process ds = 
      event_map <- IdMap.empty; (* reset context before running *)
      self#visit_decls () ds;
      
    method! visit_DEvent () id x y z params = 
      (* if the event has a polymorphic type 
         in any of its arguments, add it to the list *)
      if List.exists (fun (_, ty) -> is_polymorphic_ty ty) params then (
        event_map <- IdMap.add id {id; params; ecalls=[]} event_map
      );
      super#visit_DEvent () id x y z params

    method! visit_PCall _ pcall_arg = 
      PCall(pcall_arg) (* need to skip manually because the arg is type event *)

    method! visit_PGen _ pgen_arg = 
      (* print_endline ("[event_ctor_replacer] visiting PGen with args: "^(Printing.exp_to_string pgen_arg));
      print_endline ("[event_ctor_replacer] pgen_arg type: "^(Option.get pgen_arg.ety |> Printing.ty_to_string));
      (match pgen_arg.e, (Option.get pgen_arg.ety).raw_ty with 
        | ECall(_), TEvent -> print_endline ("[event_ctor_replacer] pgen_arg is an ecall with type TEvent, so we should visit it...");
        | _, TEvent -> print_endline ("[event_ctor_replacer] pgen_arg has type TEvent, but is not an ecall...");
        | ECall(_), _ -> print_endline ("[event_ctor_replacer] pgen_arg is an ECall, but does not have type TEvent... ("^(Option.get pgen_arg.ety |> Printing.ty_to_string)^")");
        | _, _ -> print_endline ("[event_ctor_replacer] pgen_arg is not an ECall and does not have type TEvent... ");
      ); *)
      let pgen_arg' = self#visit_exp () pgen_arg in
      PGen(pgen_arg')


    method! visit_exp _ exp =
      (* transform event constructor calls to events in the list *)
      match exp.e, ((Option.get exp.ety) |> normalize_ty).raw_ty with 
      (* event construction expression *)
      | ECall(event_cid, args, flag), TEvent -> 
        let event_id = Cid.to_id event_cid in
        (match IdMap.find_opt event_id event_map with
          | Some edecl -> (* this is an event with a polymorphic argument. We need a monomorphic id *)
            let monomorphic_id, updated_edecl = add_concrete_sig edecl args in
            let event_cid' = Cid.id monomorphic_id in
            event_map <- IdMap.add event_id updated_edecl event_map; (* update the event declaration with the new call *)
            let exp' = {exp with e=ECall(event_cid', args, flag)} in
            super#visit_exp () exp' (* visit the new expression to find nested event constructor calls *)
          | None -> (* this is not an event with a polymorphic argument, only need to recurse *)
            super#visit_exp () exp
        )
      | _ -> super#visit_exp () exp (* super to skip / prevent infinite recursion *)    
        
  end in
  let ds = obj#process ds in
  obj#event_map, ds
;;


(* make a concrete version of an event decl, based on concrete_sig *)
let concrete_event_decl decl _ num_opt esort specs params concrete_sig =
  let params' = List.mapi (fun i (param_id, _) -> (param_id, List.nth concrete_sig.concrete_tys i)) params in
  { decl with d = DEvent(concrete_sig.id, num_opt, esort, specs, params') }
;;

let concrete_event_decls decl id num_opt esort specs params concrete_sigs =
  List.map (fun concrete_sig -> concrete_event_decl decl id num_opt esort specs params concrete_sig) concrete_sigs
;;

let concrete_handler_decl decl _ hsort (params, stmt) concrete_sig =
  let params' = List.mapi (fun i (param_id, _) -> (param_id, List.nth concrete_sig.concrete_tys i)) params in
  { decl with d = DHandler(concrete_sig.id, hsort, (params', stmt)) }
;;
let concrete_handler_decls decl id hsort (params, stmt) concrete_sigs =
  List.map (fun concrete_sig -> concrete_handler_decl decl id hsort (params, stmt) concrete_sig) concrete_sigs
;;

let update_decls event_map ds = 
  (* use an object to visit DEvents and DHandlers and replace with monomorphic ones according to the event map *)
  let obj = object (self)
    inherit [_] s_map as super
    method process event_map ds = self#visit_decls event_map ds
    method! visit_decls event_map decls = 
      match decls with 
      | [] -> []
      | decl::decls -> (
        (* visit rest (decl may be a module, and need to visit the rest) *)
        let decl = self#visit_decl event_map decl in
        let decls' = self#visit_decls event_map decls in
        match decl.d with 
        | DEvent(id, _,  esort, specs, params) -> (
          match IdMap.find_opt id event_map with
          | Some edecl -> (
            let new_decls = concrete_event_decls decl id None esort specs params edecl.ecalls in
            new_decls@decls'
          )
          | None -> decl::decls'
        )
        | DHandler(id, hsort, (params, stmt)) -> (
          match IdMap.find_opt id event_map with
          | Some edecl -> (
            let new_decls = concrete_handler_decls decl id hsort (params, stmt) edecl.ecalls in
            new_decls@decls'
          )
          | None -> decl::decls'
        )
        | _ -> decl::decls'
      )
  end in
  obj#process event_map ds
;;

let eliminate_prog ds = 
  let emap, ds' = update_calls ds in
  (* update declarations to be monomorphic *)
  (* left off here *)
  let ds'' = update_decls emap ds' in
  (* update event numbers *)
  (* LEFT OFF HERE: event numbers should be cleared on monomorphization *)
  let rv = EventFormat.set_event_nums ds'' in 
  print_endline "---------- Output decls -----------";
  Printing.decls_to_string rv |> print_endline;
  print_endline "---------- Output decls -----------";
  rv 
;;


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

let concrete_sig_equal (sig1 : concrete_sig) (sig2 : concrete_sig) = 
  (* print_endline ("[concrete_sig_equal] comparing concrete sigs: " ^ (Id.name sig1.id) ^ " vs " ^ (Id.name sig2.id)); *)
  let equiv_ids = Id.equal sig1.id sig2.id in
  let equiv_tys = equiv_lists (equiv_ty ~ignore_effects:true ~qvars_wild:true) sig1.concrete_tys sig2.concrete_tys in
  (* if not equiv_ids then
    print_endline ("\t[concrete_sig_equal] concrete_sig_equal: ids not equal: " ^ (Id.name sig1.id) ^ " vs " ^ (Id.name sig2.id));
  if not equiv_tys then
    print_endline ("\t[concrete_sig_equal] concrete_sig_equal: tys not equal: " ^ (Printing.list_to_string Printing.ty_to_string sig1.concrete_tys) ^ " vs " ^ (Printing.list_to_string Printing.ty_to_string sig2.concrete_tys));
  if equiv_ids && equiv_tys then
    print_endline ("\t[concrete_sig_equal] concrete_sig_equal: sigs are equal!"); *)
  equiv_ids && equiv_tys
  (* Id.equal sig1.id sig2.id &&
  equiv_lists (equiv_ty ~ignore_effects:true ~qvars_wild:true) sig1.concrete_tys sig2.concrete_tys *)
;;
(* everything about an event and handler declaration *)
type event_decl = {
  id:id; 
  params : params;
  ecalls : concrete_sig list; (* calls seen so far *)
}
type event_map = event_decl IdMap.t


let event_decl_equal (edecl1 : event_decl) (edecl2 : event_decl) = 
  (* print_endline ("[event_decl_equal] comparing event decls: " ^ (Id.name edecl1.id) ^ " vs " ^ (Id.name edecl2.id)); *)
  let equiv_ids = Id.equal edecl1.id edecl2.id in
  let equiv_params = equiv_lists 
    (fun (id1, ty1) (id2, ty2) -> Id.equal id1 id2 && equiv_ty ~ignore_effects:true ~qvars_wild:true ty1 ty2) 
    edecl1.params 
    edecl2.params in
  (* let ecalls_len1 = List.length edecl1.ecalls in
  let ecalls_len2 = List.length edecl2.ecalls in *)
  (* print_endline ("\t[even_decl_equal] edecl1 has "^ (string_of_int ecalls_len1) ^ " calls, edecl2 has "^ (string_of_int ecalls_len2) ^ " calls"); *)
  let equiv_ecalls = equiv_lists concrete_sig_equal edecl1.ecalls edecl2.ecalls in
  (* if not equiv_ids then
    print_endline ("[event_decl_equal] event_decl_equal: ids not equal: " ^ (Id.name edecl1.id) ^ " vs " ^ (Id.name edecl2.id));
  if not equiv_params then
    print_endline ("[event_decl_equal] event_decl_equal: params not equal: " ^ (Printing.list_to_string (fun (id, ty) -> "(" ^ (Id.name id) ^ ", " ^ (Printing.ty_to_string ty) ^ ")") edecl1.params) ^ " vs " ^ (Printing.list_to_string (fun (id, ty) -> "(" ^ (Id.name id) ^ ", " ^ (Printing.ty_to_string ty) ^ ")") edecl2.params));
  if not equiv_ecalls then
    print_endline ("[event_decl_equal] event_decl_equal: ecalls not equal"); *)
  equiv_ids && equiv_params && equiv_ecalls
  (* Id.equal edecl1.id edecl2.id &&
  equiv_lists 
    (fun (id1, ty1) (id2, ty2) -> Id.equal id1 id2 && equiv_ty ~ignore_effects:true ~qvars_wild:true ty1 ty2) 
    edecl1.params 
    edecl2.params 
  && equiv_lists concrete_sig_equal edecl1.ecalls edecl2.ecalls *)
;;

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
    (* print_endline ("[event_ctor_replacer] adding new concrete instance for event "^ (Id.name event_decl.id) ^ " with arg types: " ^ (Printing.list_to_string Printing.ty_to_string arg_tys)); *)
    let id' = Id.create ((Id.name (event_decl.id)) ^ "_" ^ string_of_int (List.length event_decl.ecalls)) in
    let exp_to_rawty exp = (Option.get exp.ety) in
    let concrete_tys = List.map exp_to_rawty call_args in 
    let ecalls' = event_decl.ecalls@[{id=id'; concrete_tys}] in
    id', {event_decl with ecalls = ecalls'}
;;

(* New main function *)
let update_calls emap ds : event_decl IdMap.t * decls = 
  (* use an object to visit DEvents and find all event decls *)
  let obj = object (self)
    inherit [_] s_map as super
    val mutable event_map = IdMap.empty
    method event_map = event_map
    (* entry point *)
    method process emap ds = 
      event_map <- emap; (* reset context before running *)
      self#visit_decls () ds;
      
    method! visit_DEvent () id x y z params = 
      (* if the event has a polymorphic type 
         in any of its arguments, add it to the list *)
      (* only add if its not already there *)
      if List.exists (fun (_, ty) -> is_polymorphic_ty ty) params then (
        if IdMap.mem id event_map then(
          (* print_endline ("[event_ctor_replacer] warning: duplicate event declaration for "^ (Id.name id) ^ " with polymorphic parameters. This may cause issues with monomorphization."); *)
          ())
        else
        event_map <- IdMap.add id {id; params; ecalls=[]} event_map
      );
      super#visit_DEvent () id x y z params

    method! visit_PCall _ pcall_arg = 
      PCall(pcall_arg) (* need to skip manually because the arg is type event *)

    method! visit_PGen _ pgen_arg = 
      (* print_endline ("[event_ctor_replacer] visiting PGen with args: "^(Printing.exp_to_string pgen_arg)); *)
      (* print_endline ("[event_ctor_replacer] pgen_arg type: "^(Option.get pgen_arg.ety |> Printing.ty_to_string));
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
      let _ = match exp.ety with 
        | Some ty -> ty
        | None -> failwith ("[event_ctor_replacer] found expression without type annotation: "^(Printing.exp_to_string exp))
      in
      match exp.e, ((Option.get exp.ety) |> normalize_ty).raw_ty with 
      (* event combinator *)
      | ECall(event_cid, _, _), TEvent when Cid.equal_names event_cid (Cid.create ["Event"; "delay"]) -> 
        super#visit_exp () exp (* continue to inner event *)
      | ECall(event_cid, args, flag), TEvent -> 
        (* check if it is a builtin event combinator, for which we recurse *)
          (* print_endline ("[event_ctor_replacer] ECall event_cid = "^(Printing.cid_to_string event_cid));
          print_endline ("[event_ctor_replacer] visiting ECall with args: "^(Printing.list_to_string Printing.exp_to_string args)); *)
        let event_id = Cid.to_id event_cid in
        (match IdMap.find_opt event_id event_map with
          | Some edecl -> (* this is an event with a polymorphic argument. We need a monomorphic id *)
            (* if the arguments themselves are polymorphic, it doesn't define a monomorphic call
              and we will need to replace it with the appropriate monomorphic instance later *)
            let args_are_polymorphic = List.exists (fun arg -> is_polymorphic_ty (Option.get arg.ety)) args in
            if args_are_polymorphic then (
              print_endline ("[event_ctor_replacer] arguments are polymorphic, so we will not replace this call with a monomorphic one...");
              super#visit_exp () exp 
            )
            else (
            (* print_endline ("[event_ctor_replacer] Found event constructor call for event "^ (Id.name event_id) ^ " with polymorphic params, replacing with monomorphic event constructor call..."); *)
            let monomorphic_id, updated_edecl = add_concrete_sig edecl args in
            let event_cid' = Cid.id monomorphic_id in
            event_map <- IdMap.add event_id updated_edecl event_map; (* update the event declaration with the new call *)
            let exp' = {exp with e=ECall(event_cid', args, flag)} in
            (* print_endline ("[event_ctor_replacer] new ECall exp: "^(Printing.exp_to_string exp')); *)
            super#visit_exp () exp' (* visit the new expression to find nested event constructor calls *)
          )
          | None -> (* this is not an event with a polymorphic argument, only need to recurse *)
            super#visit_exp () exp
        )
      | _ -> super#visit_exp () exp (* super to skip / prevent infinite recursion *)    
        
  end in
  let ds = obj#process emap ds in
  obj#event_map, ds
;;


(* make a concrete version of an event decl, based on concrete_sig *)
(* all arguments besides the last are data of the base polymorphic instance *)
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
            decl::new_decls@decls' (* leave the event here for now, for type inference *)
          )
          | None -> decl::decls'
        )
        | DHandler(id, hsort, (params, stmt)) -> (
          match IdMap.find_opt id event_map with
          | Some edecl -> ( (* edecls is all the info about this event *)
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

let delete_polymorphic_event_decls ds = 
  (* use an object to visit DEvents and delete those with polymorphic parameters *)
  let obj = object (self)
    inherit [_] s_map as super
    method process ds = self#visit_decls () ds
    method! visit_decls _ decls = 
      match decls with 
      | [] -> []
      | decl::decls -> (
        let decls' = self#visit_decls () decls in
        match decl.d with 
        | DEvent(_, _,  _, _, params) -> 
          if List.exists (fun (_, ty) -> is_polymorphic_ty ty) params 
          then decls'       (* delete this declaration *)
          else decl::decls' (* keep this declaration *)
        | _ -> decl::decls'
      )
  end in
  obj#process ds

(* ============================================================ *)
(*  Parser monomorphization                                      *)
(*                                                               *)
(*  Parsers can declare polymorphic parameters (e.g., `auto`)    *)
(*  the same way events can, and they can be invoked from        *)
(*  other parsers via `PCall`. Because parsers are non-recursive *)
(*  and every control-flow path ends in `generate` or `drop`,    *)
(*  we can monomorphize them by the same scheme used for events: *)
(*  for each PCall to a polymorphic parser, materialize a        *)
(*  concrete copy keyed by the arg-type signature.               *)
(*                                                               *)
(*  This must run *before* event monomorphization, so that by    *)
(*  the time the event pass scans `generate` statements inside   *)
(*  duplicated parser bodies, those statements have concrete     *)
(*  argument types.                                              *)
(* ============================================================ *)

type parser_decl = {
  pid : id;
  pparams : params;
  pcalls : concrete_sig list;
  (* prefix length of pcalls that has already been materialized as DParser
     decls in the program. Each fixpoint iteration emits only the suffix
     past this index, then bumps it. *)
  nemitted : int;
}
type parser_map = parser_decl IdMap.t

(* Convergence check: ignores nemitted, since that's bookkeeping. *)
let parser_pcalls_equal (p1 : parser_decl) (p2 : parser_decl) =
  Id.equal p1.pid p2.pid
  && equiv_lists concrete_sig_equal p1.pcalls p2.pcalls
;;

(* Look up or create a concrete instance of a polymorphic parser for the
   given call's arg types. *)
let add_concrete_parser_sig parser_decl call_args : id * parser_decl =
  let arg_tys = List.map (fun exp -> Option.get exp.ety) call_args in
  let fst_matching_call_opt =
    List.find_opt
      (fun (call : concrete_sig) ->
        equiv_lists (equiv_ty ~ignore_effects:true) arg_tys call.concrete_tys)
      parser_decl.pcalls
  in
  match fst_matching_call_opt with
  | Some call -> call.id, parser_decl
  | None ->
    let id' =
      Id.create
        (Id.name parser_decl.pid
         ^ "_"
         ^ string_of_int (List.length parser_decl.pcalls))
    in
    let concrete_tys = List.map (fun exp -> Option.get exp.ety) call_args in
    let pcalls' = parser_decl.pcalls @ [{ id = id'; concrete_tys }] in
    id', { parser_decl with pcalls = pcalls' }
;;

(* Walk the program; for each PCall to a polymorphic parser whose call-site
   args are concrete, rewrite the call's parser id to a (possibly new)
   monomorphic instance and record that instance in the parser_map. *)
let update_parser_calls pmap ds : parser_map * decls =
  let obj =
    object (self)
      inherit [_] s_map as super
      val mutable parser_map = IdMap.empty
      method parser_map = parser_map

      method process pmap ds =
        parser_map <- pmap;
        self#visit_decls () ds

      method! visit_DParser () id params block =
        if List.exists (fun (_, ty) -> is_polymorphic_ty ty) params then begin
          if not (IdMap.mem id parser_map) then
            parser_map
              <- IdMap.add id { pid = id; pparams = params; pcalls = []; nemitted = 0 } parser_map
        end;
        super#visit_DParser () id params block

      method! visit_PCall () pcall_arg =
        match pcall_arg.e with
        | ECall (parser_cid, args, flag) ->
          let parser_id = Cid.to_id parser_cid in
          (match IdMap.find_opt parser_id parser_map with
           | Some pdecl ->
             let args_are_polymorphic =
               List.exists
                 (fun arg -> is_polymorphic_ty (Option.get arg.ety))
                 args
             in
             if args_are_polymorphic
             then super#visit_PCall () pcall_arg
             else begin
               let monomorphic_id, updated_pdecl =
                 add_concrete_parser_sig pdecl args
               in
               parser_map <- IdMap.add parser_id updated_pdecl parser_map;
               let pcall_arg' =
                 { pcall_arg with e = ECall (Cid.id monomorphic_id, args, flag) }
               in
               super#visit_PCall () pcall_arg'
             end
           | None -> super#visit_PCall () pcall_arg)
        | _ -> super#visit_PCall () pcall_arg
    end
  in
  let ds = obj#process pmap ds in
  obj#parser_map, ds
;;

(* Build a concrete copy of a polymorphic parser decl. Param identifiers are
   preserved; only their types are replaced with the concrete sig types. The
   body is left untouched and will be re-typed against the new params. *)
let concrete_parser_decl decl params block (cs : concrete_sig) =
  let params' =
    List.mapi
      (fun i (param_id, _) -> param_id, List.nth cs.concrete_tys i)
      params
  in
  { decl with d = DParser (cs.id, params', block) }
;;

let concrete_parser_decls decl params block concrete_sigs =
  List.map (fun cs -> concrete_parser_decl decl params block cs) concrete_sigs
;;

(* Emit one DParser per concrete sig collected so far, but skip any sig that
   was already emitted in a prior fixpoint iteration (tracked via nemitted).
   The original polymorphic decl is left in place until after re-typing so
   the typer can still find it. *)
let update_parser_decls parser_map ds =
  let obj =
    object (self)
      inherit [_] s_map as super
      method process pmap ds = self#visit_decls pmap ds

      method! visit_decls pmap decls =
        match decls with
        | [] -> []
        | decl :: rest ->
          let decl = self#visit_decl pmap decl in
          let rest' = self#visit_decls pmap rest in
          (match decl.d with
           | DParser (id, params, block) ->
             (match IdMap.find_opt id pmap with
              | Some pdecl ->
                let pending = BatList.drop pdecl.nemitted pdecl.pcalls in
                let new_decls =
                  concrete_parser_decls decl params block pending
                in
                (decl :: new_decls) @ rest'
              | None -> decl :: rest')
           | _ -> decl :: rest')
    end
  in
  obj#process parser_map ds
;;

(* Mark every pcall in the map as emitted. Called after update_parser_decls
   so the next fixpoint iteration won't re-emit the same decls. *)
let mark_emitted (pmap : parser_map) : parser_map =
  IdMap.map
    (fun pdecl -> { pdecl with nemitted = List.length pdecl.pcalls })
    pmap
;;

let delete_polymorphic_parser_decls ds =
  let obj =
    object (self)
      inherit [_] s_map as super
      method process ds = self#visit_decls () ds

      method! visit_decls _ decls =
        match decls with
        | [] -> []
        | decl :: rest ->
          let rest' = self#visit_decls () rest in
          (match decl.d with
           | DParser (_, params, _) ->
             if List.exists (fun (_, ty) -> is_polymorphic_ty ty) params
             then rest'
             else decl :: rest'
           | _ -> decl :: rest')
    end
  in
  obj#process ds
;;

(* Run parser monomorphization to a fixpoint. Each iteration:
     1. update_parser_calls: walk the program, rewrite PCalls to polymorphic
        parsers whose call-site args are now concrete, recording the new
        concrete sigs in pmap.
     2. If pcalls didn't grow, we've converged.
     3. Otherwise emit DParsers for the new sigs (update_parser_decls skips
        sigs already emitted in prior iterations), retype, and loop.
   Termination is bounded by the parser call-chain depth, since parsers are
   non-recursive. The max_iters cap is a safety net. *)
let monomorphize_parsers builtin_tys ds =
  let max_iters = 100 in
  let rec loop pmap ds iter =
    if iter > max_iters
    then
      failwith
        (Printf.sprintf
           "[MonomorphicEventArgs] parser monomorphization did not converge in \
            %d iterations"
           max_iters);
    let pmap', ds = update_parser_calls pmap ds in
    if IdMap.equal parser_pcalls_equal pmap pmap'
    then ds
    else begin
      let ds = update_parser_decls pmap' ds in
      let pmap' = mark_emitted pmap' in
      let ds = RefreshTypes.refresh_prog ds in
      let ds = Typer.infer_prog builtin_tys ds in
      loop pmap' ds (iter + 1)
    end
  in
  let ds = loop IdMap.empty ds 1 in
  let ds = delete_polymorphic_parser_decls ds in
  let ds = RefreshTypes.refresh_prog ds in
  let ds = Typer.infer_prog builtin_tys ds in
  ds
;;

let eliminate_prog builtin_tys ds =
  let ds = RefreshTypes.refresh_prog ds in
  let ds = Typer.infer_prog builtin_tys ds in

  (* monomorphize parsers first, so that any polymorphic events referenced by
     parser bodies get concrete arg types in the duplicated parser copies *)
  let ds = monomorphize_parsers builtin_tys ds in

  (* collect monomorphic calls (and replace their event names) *)
  let emap, ds = update_calls IdMap.empty ds in
  (* replace polymorphic declarations with monomorphic copies *)
  let ds = update_decls emap ds in

  (* type check to infer all the polymorphic args inside of event calls  *)
  let ds = RefreshTypes.refresh_prog ds in
  let ds = Typer.infer_prog builtin_tys ds in

  (* run the call collector / updator again, for all the calls in the monomorphic
     generated handlers, which are no longer polymorphic *)
  (* print_endline "------ Monomorphization second pass -------"; *)
  let emap', ds = update_calls emap ds in
  (* For now, only support cases where the second pass does not identify new 
     monomorphic instances. TODO: find and think through edge case where that may happen. *)
  (* print_endline "---------- current prog -----------"; *)
  (* Printing.decls_to_string ds |> print_endline; *)
  (* print_endline "---------- current prog -----------"; *)
  let no_changes = IdMap.equal event_decl_equal emap emap' in
  if not no_changes then
    failwith "[MonomorphicEventArgs] elimination of polymorphic event encountered\
    transitive polymorphism that requires more than 2 passes. This is not yet supported.";
  ignore emap';

    (* print_endline "-------- program at debug point --------"; *)
  (* Printing.decls_to_string ds |> print_endline; *)
  (* print_endline "-------- program at debug point --------"; *)

  (* delete the polymorphic event declarationss, which were left for type checking *)
  let ds = delete_polymorphic_event_decls ds in

  (* reset event numbers *)
  let ds = EventFormat.set_event_nums ds in 

  (* ensure all names are unique (TODO: should be taken care of inside the event / handler copying function) *)
  let renaming, ds = Renaming.rename ds in
  let ds = RefreshTypes.refresh_prog ds in
  let ds = Typer.infer_prog builtin_tys ds in
  let ds = RefreshTypes.refresh_prog ds in

  (* print_endline "---------- Output decls -----------";
  Printing.decls_to_string ds |> print_endline;
  print_endline "---------- Output decls -----------"; *)
  (* exit 1; *)
  renaming, ds
;;


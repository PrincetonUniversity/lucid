open Batteries
open Syntax
open SyntaxUtils
open Collections

(* 
  Eliminate polymorphic event arguments. 
  For each event kind E with 1 or more polymorphic arguments: 
    1. find all event values of kind E constructed in the program
    2. construct a map from event_param_ty -> unique monomorphic_event_name
    3. replace the polymorphic event declaration with all the concrete ones from 2's values
    4. replace each event constructor with the appropriate one from 2
        - Also have to replace handlers!
*)

(* TODO: 
    Everything needs to be filtered on events that have polymorphic parameters. 
    This can only be known from looking at the declaration, so it must be done before
    processing the event constructor calls. 
    And the constructor call replacer and declaration replacer 
    must be aware of the filtering, so they only update the relevant events.
*)

(* event id -> event arg types *)
module IdMap = Collections.IdMap


type event_call = {id : id; args : exp list;}

(* everything about an event and handler declaration *)
type event_decl = {
  id:id; 
  num_opt : int option; 
  esort : event_sort; 
  specs : constr_spec list; 
  params : params;
  hid: id; (* handler id *)
  hsort : handler_sort;
  hbody : body;
  ecalls : event_call list; (* all the constructor calls for this event, 
                               for context when creating monomorphic decls *)
}

let event_decl_to_string (edecl : event_decl) : string = 
  let param_strs = List.map (fun (id, ty) -> (Id.to_string id)^":"^(Printing.ty_to_string ty)) edecl.params in
  let params_str = String.concat ", " param_strs in
  Printf.sprintf "event %s(%s) { handler: %s(%s) { ... } }" 
    (Id.to_string edecl.id) 
    params_str 
    (Id.to_string edecl.hid)
    (String.concat ", " (List.map (fun (id, ty) -> (Id.to_string id)^":"^(Printing.ty_to_string ty)) (fst edecl.hbody)))
    (* (Printing.stmt_to_string (snd edecl.hbody)) *)
;;

let is_polymorphic (edecl : event_decl) : bool = 
  List.exists (fun (_, ty) -> match ty.raw_ty with 
    | TQVar _ -> true
    | _ -> false) edecl.params
;;

let get_poly_params (edecl : event_decl) : (id * ty) list = 
  List.filter (fun (_, ty) -> match ty.raw_ty with 
    | TQVar _ -> true
    | _ -> false) edecl.params
;;


type event_map = event_decl IdMap.t

(* get all the event declarations *)
let get_events ds : event_decl IdMap.t = 
  (* use an object to visit DEvents and find all event decls *)
  let obj = object (self)
    inherit [_] s_iter as super
    val mutable event_map = IdMap.empty
    method event_map = event_map
    method process ds = 
      event_map <- IdMap.empty; (* reset context before running *)
      self#visit_decls () ds;
      event_map
    method! visit_DEvent _ id num_opt esort specs params = 
      let decl = {id; num_opt; esort; specs; params; hid=Id.create "none"; hsort=HData; hbody=([], snoop); ecalls=[]} in
      event_map <- IdMap.add id decl event_map
    method! visit_DHandler _ id hsort (params, body) = 
      (* update the corresponding event decl with handler info *)
      let decl = IdMap.find id event_map in
      let decl' = {decl with hid=id; hsort=hsort; hbody=(params, body)} in
      event_map <- IdMap.add id decl' event_map
  end in
  obj#process ds;
;;



let test ds = 
  let emap = get_events ds in
  (* print out every event declaration *)
  IdMap.iter (fun _ decl -> print_endline (event_decl_to_string decl)) emap;
  exit 1;
;;





type event_ctor_call = exp list
type event_ctor_call_map = (event_ctor_call list) IdMap.t
(* put an event constructor call into the appropriate entry *)
let emplace_event_ctor_call (event_cid : Cid.t) (args : exp list) (m : event_ctor_call_map) : event_ctor_call_map =
  let event_id = Cid.to_id event_cid in
  match IdMap.find_opt event_id m with
    | None -> IdMap.add event_id [args] m
    | Some existing_calls -> IdMap.add event_id (args :: existing_calls) m
;;
let equiv_rtys (tys1 : raw_ty list) (tys2 : raw_ty list) : bool = 
  let equal = SyntaxUtils.equiv_raw_ty ~ignore_effects:false ~qvars_wild:false in
  List.length tys1 = List.length tys2 &&
  List.for_all2 (fun ty1 ty2 -> equal ty1 ty2) tys1 tys2
;;
let unique_ctor_calls (calls : event_ctor_call list) : event_ctor_call list = 
  let exp_to_rawty exp = Option.get exp.ety |> fun t -> t.raw_ty in
  let unique_calls = List.fold_left 
    (fun acc call -> 
      if List.exists 
        (fun c -> 
          equiv_rtys 
            (List.map exp_to_rawty c) 
            (List.map exp_to_rawty call)) 
        acc 
      then acc 
      else call :: acc) 
    [] 
    calls 
  in
  unique_calls
;;
let unique_event_ctor_calls (m : event_ctor_call_map) : event_ctor_call_map = 
  IdMap.map unique_ctor_calls m
;;

(* generate the monomorphic event id given the id of 
   a polymorphic event and the type signature of one instance *)
(* Edge case: higher-order events *)

(* convert a typed expression in an event construction expressing into a string *)
let rec concrete_exp_to_raw_ty_str (exp : exp) : string = 
  match exp.e,(Option.get exp.ety).raw_ty with 
  | _, TQVar _ -> failwith "concrete_exp_to_raw_ty_str should only be given expressions with concrete types"
  | _, TBool -> "bool"
  | _, TVoid -> failwith "[MonomorphicEventArgs] void type not supported as event arg"
  | _, TGroup -> failwith "not supported as event arg"
  | _, TInt (sz) -> "int"^Printing.size_to_string sz  
  | ECall(event_cid, args, _), TEvent -> 
    let arg_strs = List.map concrete_exp_to_raw_ty_str args in
    "event__"^Cid.to_string event_cid ^"__"^ (String.concat "_" arg_strs) ^"__"
  | _, TEvent -> failwith "[MonomorphicEventArgs] event type not supported as event arg"
  | _, TFun _  -> failwith "tfun not supported as event arg"
  | _, TMemop _ -> failwith "tmemop not supported as event arg"
  | _, TName (cid, szs, _) -> Cid.to_string cid^"__"^(String.concat "_" (List.map Printing.size_to_string szs))^"__"
  | _, TAbstract (cid, szs, _, _) -> Cid.to_string cid^"__"^(String.concat "_" (List.map Printing.size_to_string szs))^"__"
  | ERecord(fname_exps), TRecord(_) ->
    "record__"^(String.concat "_" (List.map (fun (fname, exp) -> fname^concrete_exp_to_raw_ty_str exp) fname_exps))^"__"
  | _, TRecord(_) -> failwith "[MonomorphicEventArgs] record type with non-record expression"
  | EVector (exps), TVector(_, len) -> 
    "vector__"^(concrete_exp_to_raw_ty_str (List.hd exps))^(Printing.size_to_string len)^"__"
  | _, TVector(_) -> failwith "[MonomorphicEventArgs] vector type with non-vector expression"
  | ETuple(exps), TTuple(_) -> 
    "tuple__"^(List.map concrete_exp_to_raw_ty_str exps |> String.concat "_")^"__"
  | _, TTuple(_) -> failwith "[MonomorphicEventArgs] tuple type with non-tuple expression"
  | _, TTable _  -> failwith "ttable not supported as event arg"
  | _, TBuiltin _ -> failwith "TODO: support builtin types as polymorphic event args"
  | _, TAction _ -> failwith "taction not supported as event arg"
  | _, TActionConstr _ -> failwith "tactionconstr not supported as event arg"
  | _, TPat _  -> failwith "tpat not supported as event arg"
  | _, TBitstring -> failwith "tbitstring not supported as event arg"
;;

let monomorphic_event_id id args = 
  print_endline ("[monomorphic_event_id] generating monomorphic event id for event "^Id.to_string id^" with args: "^(Printing.comma_sep Printing.exp_to_string args));
  let arg_strs = List.map concrete_exp_to_raw_ty_str args in
  Id.create (Id.to_string id ^"__"^ (String.concat "_" arg_strs) ^"__")
;;

let event_ctor_replacer = 
  (* visit each event constructor call, replace event ID, and
     collect per-event-id calls for declaration updates *)
  object (self)
    inherit [_] s_map as super
    val mutable ctx = IdMap.empty
    method ctx = ctx
    method process ds = 
      ctx <- IdMap.empty; (* reset context before running *)
      let ds' = self#visit_decls () ds in
      ds', unique_event_ctor_calls ctx

    method! visit_PCall _ pcall_arg = 
      (* PCall is calling another parser function, which has type TEvent
         we want to skip this *)
      PCall(pcall_arg)
    method! visit_PGen _ pgen_arg = 
      print_endline ("[event_ctor_replacer] visiting PGen with args: "^(Printing.exp_to_string pgen_arg));
      print_endline ("[event_ctor_replacer] pgen_arg type: "^(Option.get pgen_arg.ety |> Printing.ty_to_string));
      (match pgen_arg.e, (Option.get pgen_arg.ety).raw_ty with 
        | ECall(_), TEvent -> print_endline ("[event_ctor_replacer] pgen_arg is an ecall with type TEvent, so we should visit it...");
        | _, TEvent -> print_endline ("[event_ctor_replacer] pgen_arg has type TEvent, but is not an ecall...");
        | ECall(_), _ -> print_endline ("[event_ctor_replacer] pgen_arg is an ECall, but does not have type TEvent... ("^(Option.get pgen_arg.ety |> Printing.ty_to_string)^")");
        | _, _ -> print_endline ("[event_ctor_replacer] pgen_arg is not an ECall and does not have type TEvent... ");
      );
      let pgen_arg' = self#visit_exp () pgen_arg in
      PGen(pgen_arg')
    method! visit_exp _ exp =
      match exp.e, ((Option.get exp.ety) |> normalize_ty).raw_ty with 
      (* event construction expression *)
      | ECall(event_cid, args, flag), TEvent -> 
        print_endline("[event_ctor_replacer] visiting TEvent ECall with cid: "^Cid.to_string event_cid^" and args: "^(Printing.comma_sep Printing.exp_to_string args));
        let args = List.map (self#visit_exp ()) args in
        (* update the map, for creating the monomorphic events declarations later *)
        let event_id = Cid.to_id event_cid in
        ctx <- emplace_event_ctor_call event_cid args ctx;
        (* replace with the appropriate monomorphic event constructor *)
        let new_event_id = monomorphic_event_id event_id args in
        let new_event_cid = Cid.id new_event_id in
        {exp with e=ECall(new_event_cid, args, flag)}
      | _ -> 
        print_endline ("[event_ctor_replacer] skipping expression: "^(Printing.exp_to_string exp)^" with type: "^(Option.get exp.ety |> Printing.ty_to_string));        
        super#visit_exp () exp (* super to skip / prevent infinite recursion *)    
  end
;;

(* replace an event or handler declaration with a list of monomorphic *)
let monomorphic_devent decl call_args = 
  match decl.d with 
  | DEvent(event_id, num_opt, ev_sort, constr_specs, params) -> (
    (* update parameter types (so they are all concrete) *)
    let tys = List.map (fun exp -> (Option.get exp.ety)) call_args in
    let params' = List.mapi (fun i (id, _) -> (id, List.nth tys i)) params in
    (* calculate new event id *)
    let event_id' = monomorphic_event_id event_id call_args in
    (* return new declaration *)
    {decl with d=DEvent(event_id', num_opt, ev_sort, constr_specs, params')}
  )
  | DHandler(hdl_id, hdl_sort, (params, stmt)) -> (
    (* update parameter types (so they are all concrete) *)
    let tys = List.map (fun exp -> (Option.get exp.ety)) call_args in
    let params' = List.mapi (fun i (id, _) -> (id, List.nth tys i)) params in
    (* calculate new event id *)
    let hdl_id' = monomorphic_event_id hdl_id call_args in
    (* return new declaration *)
    {decl with d=DHandler(hdl_id', hdl_sort, (params', stmt))}
  )
  | _ -> decl
;;

let monomorphic_devents decl call_args_list = 
  List.map (fun call_args -> monomorphic_devent decl call_args) call_args_list
;;




let event_declaration_specializer = 
  (* visit each event declaration, replace with monomorphic ones according to the context *)
  object (self)
    inherit [_] s_map as super
    method process ctx ds  = self#visit_decls ctx ds
    method! visit_decls ctx decls = 
      match decls with 
      | [] -> []
      | decl::decls -> (
        (* visit rest (decl may be a module, and need to visit the rest) *)
        let decl = self#visit_decl ctx decl in
        let decls' = self#visit_decls ctx decls in
        match decl.d with 
        (* replace event and handler decls *)
        | DEvent(id,_, _, _, _) | DHandler(id, _, _) -> 
          let call_args_list = IdMap.find id ctx in
          let devents = monomorphic_devents decl call_args_list in
          devents@decls'
        | _ -> decl::decls'
      )
  end


let eliminate_prog ds = 
  test ds;
  (* 1. replace the event ctor expressions and get context *)
  let ds', event_ctor_calls = event_ctor_replacer#process ds in
  (* 2. replace the event and handler declarations with monomorphic ones according to context *)
  let ds'' = event_declaration_specializer#process event_ctor_calls ds' in
  (* update event numbers *)
  EventFormat.set_event_nums ds''
;;


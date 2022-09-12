(* Core syntax with a few extra nodes for the Tofino. 
    Once we converge on the additional nodes, 
    we may want to add them to CoreSyntax. *) 

open CoreSyntax

exception Error of string
let error s = raise (Error s)

type id = [%import: (Id.t[@opaque])]
and cid = [%import: (Cid.t[@opqaue])]
and sp = [%import: Span.t]
and z = [%import: (Z.t[@opaque])]
and zint = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]
and location = int
(* All sizes should be inlined and precomputed *)
and size = int
and sizes = size list
and raw_ty = [%import: CoreSyntax.raw_ty]
and func_ty = [%import: CoreSyntax.func_ty]
and ty = [%import: CoreSyntax.ty]
and tys = [%import: CoreSyntax.tys]
and op = [%import: CoreSyntax.op]
and pat = [%import: CoreSyntax.pat]
and v = [%import: CoreSyntax.v]
and event = [%import: CoreSyntax.event]
and value = [%import: CoreSyntax.value]
and e = [%import: CoreSyntax.e]
and exp = [%import: CoreSyntax.exp]
and branch = [%import: CoreSyntax.branch]
and gen_type = [%import: CoreSyntax.gen_type]
and s = [%import: CoreSyntax.s]
and statement = [%import: CoreSyntax.statement]
and params = [%import: CoreSyntax.params]
and body = [%import: CoreSyntax.body]
and event_sort = [%import: CoreSyntax.event_sort]
and conditional_return = [%import: CoreSyntax.conditional_return]
and complex_body = [%import: CoreSyntax.complex_body]
and memop_body = [%import: CoreSyntax.memop_body]


(* multicast id space: 
  0 - 511: recirculated event cloning
  512 - 1024: port flooding groups
  1024 >    : user groups *)  

and event_output = {
  (* count the number of recirc / self events 
     generated on this path *)
  recirc_mcid_var : (id * ty);
  (* all possible sequences of events 
     that this program can generate. *)
  ev_gen_seqs : (id list list);
}
and main_handler = {
    main_id : id;
    hdl_selector : (id * ty);
    hdl_enum : (id * int) list;
    hdl_params : (id * params) list;
    default_hdl : id option;
    shared_locals : (id * ty) list;
    main_body : statement list;
    event_output : event_output;
    }
and memop = {mid:id; mparams:params; mbody:memop_body;}
and td =
  | TDGlobal of id * ty * exp
  | TDEvent of id * event_sort * params
  | TDHandler of id * body 
  | TDMemop of memop
  | TDExtern of id * ty
  | TDMain of main_handler
  | TDLabeledBlock of id * statement

and tdecl = {td:td; tdspan: sp;}
and tdecls = tdecl list
[@@deriving
  visitors
    { name = "s_iter"
    ; variety = "iter"
    ; polymorphic = false
    ; data = true
    ; concrete = true
    ; nude = false
    }
  , visitors
      { name = "s_map"
      ; variety = "map"
      ; polymorphic = false
      ; data = true
      ; concrete = true
      ; nude = false
      }]


let tdecl_of_decl decl = 
  match decl.d with
  | DGlobal(id, ty, exp) -> {td=TDGlobal(id, ty, exp); tdspan=decl.dspan;}
  | DEvent (id, es, ps) -> {td=TDEvent(id, es, ps); tdspan=decl.dspan;}
  | DHandler(i, b) -> {td=TDHandler(i, b); tdspan=decl.dspan;}
  | DMemop(i, p, m) -> {td=TDMemop{mid=i; mparams=p; mbody=m;}; tdspan=decl.dspan;}
  | DExtern(i, t) -> {td=TDExtern(i, t); tdspan=decl.dspan;}
;;


(* find all the possible sequences of events that get generated *)
let rec find_ev_gen_seqs statement =
  match statement.s with 
  | SGen(_, ev_exp) -> (
    match ev_exp.e with 
    | ECall(ev_cid, _) -> (
      [[Cid.to_id ev_cid]]
    )
    | _ -> error "[find_ev_gen_seqs] event should be a call by this point"
  )
  | SIf(_, s1, s2) ->
    (find_ev_gen_seqs s1)@(find_ev_gen_seqs s2)
  | SMatch(_, branches) -> 
    List.fold_left (fun seqs (_, stmt) -> seqs@(find_ev_gen_seqs stmt)) [] branches 
  | SSeq(s1, s2) -> (
    let seqs_s1 = find_ev_gen_seqs s1 in
    let seqs_s2 = find_ev_gen_seqs s2 in
    let update_seqs seqs seq = 
      List.map (fun s -> s@seq) seqs
    in 

    List.fold_left 
      (fun new_seqs_s1 seq -> 
        new_seqs_s1@(update_seqs seqs_s1 seq)
      )
      []
      seqs_s2
    )
  (* no events in rest *)
  | _ -> [[]]
;;


(* generate the main handler *)
let add_main_handler decls =
  let main_id = Id.create "main_handler" in 
  let hdl_selector = (Id.create "event_id", (ty (TInt 8))) in 
  let hdl_enum, hdl_params, default_hdl, _= 
    let acc (enum, all_params, default_hdl, cur_ev_num) dec = 
      match dec.td with 
      | TDEvent(id, ev_sort, params) -> 
        let default_hdl = match ev_sort, default_hdl with
          | EEntry _, None -> Some id
          | EEntry _, Some _ -> error "[add_main_handler] only 1 entry event is supported"
          |  _, _ -> default_hdl
         in
        (id, cur_ev_num)::enum,
        (id, params)::all_params,
        default_hdl,
        cur_ev_num+1
      | _ -> enum, all_params, default_hdl, cur_ev_num
    in 
    List.fold_left acc ([], [], None, 1) decls
  in 
  let main_body = 
    let handler_branches branches dec = 
      match dec.td with 
      | TDHandler(hdl_id, (_, stmt)) -> (
        let hdl_num = match List.assoc_opt hdl_id hdl_enum with 
          | None -> error "[generate_merged_handler] could not find handler id in enum. Do events and handlers have the same internal IDs?"
          | Some hdl_num -> 
            hdl_num
        in 
        branches@[([PNum (Z.of_int(hdl_num))], stmt)]
      )
      | _ -> branches
    in 
    let ehdl_selector = var_sp 
        (Cid.id (fst hdl_selector))
        ((snd hdl_selector))
        (Span.default)
    in     
    let branches = List.fold_left handler_branches [] decls in
    [smatch [ehdl_selector] branches]
  in 
  let rec erase_handler_bodies decls =
    match decls with 
    | [] -> [] 
    | hd::tl -> (
      match hd with
      | {td=TDHandler(i, (p, _));} -> (
        {hd with td=TDHandler(i, (p, snoop));}::(erase_handler_bodies tl)
      )
      | _ -> hd::(erase_handler_bodies tl)
    )
  in 
  let event_output = {
    recirc_mcid_var = (Id.create "recirc_mcid", (ty (TInt 16)));
    ev_gen_seqs = find_ev_gen_seqs (List.hd main_body) |> MiscUtils.unique_list_of;
  } 
  in
  let tds =(erase_handler_bodies decls)
    @[{td=
    TDMain{main_id;hdl_selector;hdl_enum;hdl_params;default_hdl; main_body; shared_locals=[];event_output;}
    ;tdspan=Span.default}]
  in
  tds 
;;

let tdecls_of_decls decls = 
  let translated_decls = List.map tdecl_of_decl decls in
  add_main_handler translated_decls
;;


(* generate the main handler, for a program where 
   the event gets compiled to a control block library *)
let add_lib_handler decls =
  let hdl_selector = (Id.create "event_id", (ty (TInt 8))) in 
  let hdl_enum, hdl_params, default_hdl, _= 
    let acc (enum, all_params, default_hdl, cur_ev_num) dec = 
      match dec.td with 
      | TDEvent(id, ev_sort, params) -> 
        let default_hdl = match ev_sort, default_hdl with
          | EEntry _, None -> Some id
          | EEntry _, Some _ -> error "[add_main_handler] only 1 entry event is supported"
          |  _, _ -> default_hdl
         in
        (id, cur_ev_num)::enum,
        (id, params)::all_params,
        default_hdl,
        cur_ev_num+1
      | _ -> enum, all_params, default_hdl, cur_ev_num
    in 
    List.fold_left acc ([], [], None, 1) decls
  in 
  let main_body, main_id = List.filter_map 
    (fun dec -> 
      match dec.td with 
      | TDHandler(hdl_id, (_, stmt)) -> Some(stmt, hdl_id)
      | _ -> None          
    )
    decls
    |> List.hd
  in
  let rec erase_handler_bodies decls =
    match decls with 
    | [] -> [] 
    | hd::tl -> (
      match hd with
      | {td=TDHandler(i, (p, _));} -> (
        {hd with td=TDHandler(i, (p, snoop));}::(erase_handler_bodies tl)
      )
      | _ -> hd::(erase_handler_bodies tl)
    )
  in 
  let event_output = {
    recirc_mcid_var = (Id.create "recirc_mcid", (ty (TInt 16)));
    ev_gen_seqs = find_ev_gen_seqs main_body |> MiscUtils.unique_list_of;
  } 
  in
  let tds =(erase_handler_bodies decls)
    @[{td=
    TDMain{main_id;hdl_selector;hdl_enum;hdl_params;default_hdl; main_body=[main_body]; shared_locals=[];event_output;}
    ;tdspan=Span.default}]
  in
  tds 
;;

let tdecls_of_decl_for_control_lib decls = 
  let translated_decls = List.map tdecl_of_decl decls in
  add_lib_handler translated_decls
;;
(* get the main handler's signature *)
let main ds =
  let main_decs = List.filter_map (fun dec -> 
    match dec.td with 
    | TDMain main_sig -> Some main_sig
    | _ -> None 
    )
    ds
  in 
  match (main_decs) with 
    | [main_hdl] -> main_hdl
    | [] -> error "[main] no main handler."
    | _ -> error "[main] more than 1 main handler."
;;

(* replace the main handler's signature *)
let update_main ds new_main_d =
  List.map 
    (fun dec -> match dec.td with 
      | TDMain _ -> {dec with td=TDMain(new_main_d);} | _ -> dec)
    ds
;;

(* add a shared local to the main handler *)
let add_shared_local ds tmp_id tmp_ty =
  (* let tmp_v  = value_to_exp (vint 0 tmp_sz) in  *)
  let tmp_e = var_sp (Cid.id tmp_id) tmp_ty Span.default in 
  let old_main = (main ds) in 
  let new_main = {old_main with 
    shared_locals=((tmp_id, tmp_ty)::old_main.shared_locals);}
  in 
  tmp_e, update_main ds new_main
;;

(* get assoc list of memops *)
let memops tds = 
  List.filter_map
  (fun dec -> match dec.td with 
    |TDMemop(m) -> Some ((m.mid, m))
    | _ -> None
  )
  tds
;;


(*** output ***)
let decl_of_tdecl tdecl = 
  match tdecl.td with
  | TDGlobal(id, ty, exp) ->  {d=DGlobal(id, ty, exp); dspan=tdecl.tdspan;}
  | TDEvent (id, es, ps) ->  {d=DEvent(id, es, ps); dspan=tdecl.tdspan;}
  | TDHandler(i, b) ->  {d=DHandler(i, b); dspan=tdecl.tdspan;}
  | TDMemop{mid=mid; mparams=mparams; mbody=mbody;} ->  {d=DMemop(mid, mparams, mbody); dspan=tdecl.tdspan;}
  | TDExtern(i, t) ->  {d=DExtern(i, t); dspan=tdecl.tdspan;}
  | _ -> error "[decl_of_tdecl] not a directly translatable decl"
;;

let main_to_string mainsig =
  "// MAIN HANDLER \n"
  ^"handler main(...){\n"
  ^((List.mapi 
        (fun i stg_stmt -> 
          "// Stage "^(string_of_int i)^"\n"
          ^(CorePrinting.statement_to_string stg_stmt))
        )
        mainsig.main_body

     |> String.concat "\n" 
   )
  ^"}"
;;


let tdecl_to_string tdec =
  match tdec.td with 
  | TDMain(mainsig) -> 
    main_to_string mainsig
  | TDLabeledBlock(id, stmt) -> 
    "labeled_statement "^(CorePrinting.id_to_string id)^"{\n"
    ^CorePrinting.statement_to_string stmt
    ^"\n}"
  | _ -> CorePrinting.decl_to_string (decl_of_tdecl tdec)
;;
let tdecls_to_string tdecs = 
  List.map tdecl_to_string tdecs |> 
  String.concat "\n"
;;

(* 
  (draft)
    To go back to core syntax: 
    1. create a handler for the multihandler with parameters: 
        handle_selector::shared_locals@(flatten handler_sigs.params)
    2. convert each existing event into an event that calls the 
       multihandler:
            handler foo(int a, int b) {
                generate multihandler(
                    (find foo multihandler.handler_sigs).hselect_key, 
                    List.map (fun hid, hsig -> if hid = id then [a; b] else hsig.hdefaultargs)
                    @sharedlocal_defaults
                    )
            }
    3. delete the shared local and multihandler
    4. convert everything else back directly. 

    Note: we could do something fancier, and extract the tables / rules relevant 
    to each handler from the multihandler. 
*)
(* let decls_of_tdecls tdecls =
    let mh_rec = 
        match (List.filter_map
            (fun tdec -> match tdec.td with 
            | TMultiHandler(mh_rec) -> Some(mh_rec)
            | _ -> None)
            tdecls) with 
        | [mh_rec] -> mh_rec
        | _ -> error "no multihandler in tdecls -- nothing to convert back."
    in 
    let shared_locals = List.filter_map
        (fun tdec -> match tdec.td with 
            | TSharedLocal(id, ty, default) -> 
               Some((id, ty), default)
            | _ -> None
        )
        tdecls
    in 
    let shared_local_params, shared_local_defaults = List.split shared_locals in
    let mh_params = (mh_rec.handler_selector::shared_local_params)
        @(List.)
    let dmultihandler = 
        handler_sp 
            multihandler.id

 *)


;;




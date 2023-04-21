(* Core syntax with a few extra nodes for the Tofino. 
  Core syntax with two new declarations that are 
  useful for organizing core Lucid code into a 
  form that is easy to translate into P4.
  The new declarations in tofinocore are: 
     1. a "main handler" is the union of all 
        handlers. It is used to represent 
        the single P4 ingress block that 
        implements all of the handlers.
     2. a "labeled statement" that can be 
        executed by calling the label. 
        It is basically an argumentless 
        function with dynamically scoped 
        variables. 
        It is used to represent P4 actions
        that are sets of statements.

  Open question: do we want to add these nodes 
  directly to coreSyntax? They are generally 
  useful for translation into P4, but may not 
  be useful for translation to non-P4 targets. *) 

open CoreSyntax
module Ctx = Collections.CidMap

exception Error of string
let error s = raise (Error s)

type id = [%import: (Id.t[@opaque])]
and cid = [%import: (Cid.t[@opqaue])]
and tagval = [%import: (TaggedCid.tagval[@opqaue])]
and tcid = [%import: (TaggedCid.t[@opqaue])]
and sp = [%import: Span.t]
and z = [%import: (Z.t[@opaque])]
and zint = [%import: (Integer.t[@with Z.t := (Z.t [@opaque])])]
and location = int
(* All sizes should be inlined and precomputed *)
and size = int
and sizes = size list
and raw_ty = [%import: CoreSyntax.raw_ty]
and tbl_ty = [%import: CoreSyntax.tbl_ty]
and acn_ty = [%import: CoreSyntax.acn_ty]
and func_ty = [%import: CoreSyntax.func_ty]
and ty = [%import: CoreSyntax.ty]
and tys = [%import: CoreSyntax.tys]
and op = [%import: CoreSyntax.op]
and pat = [%import: CoreSyntax.pat]
and v = [%import: CoreSyntax.v]
and event = [%import: CoreSyntax.event]
and value = [%import: CoreSyntax.value]
and pragma = [%import: CoreSyntax.pragma]
and e = [%import: CoreSyntax.e]
and exp = [%import: CoreSyntax.exp]
and branch = [%import: CoreSyntax.branch]
and gen_type = [%import: CoreSyntax.gen_type]
and s = [%import: CoreSyntax.s]
and tbl_def = [%import: CoreSyntax.tbl_def]
and tbl_match_out_param = [%import: CoreSyntax.tbl_match_out_param]
and tbl_match = [%import: CoreSyntax.tbl_match]
and tbl_entry = [%import: CoreSyntax.tbl_entry]
and statement = [%import: CoreSyntax.statement]
and params = [%import: CoreSyntax.params]
and body = [%import: CoreSyntax.body]
and event_sort = [%import: CoreSyntax.event_sort]
and handler_sort = [%import: CoreSyntax.handler_sort]
and conditional_return = [%import: CoreSyntax.conditional_return]
and complex_body = [%import: CoreSyntax.complex_body]
and memop_body = [%import: CoreSyntax.memop_body]
and memop = [%import: CoreSyntax.memop]
and action_body = [%import: CoreSyntax.action_body]
and action = [%import: CoreSyntax.action]
(* multicast id space: 
  0 - 511: recirculated event cloning
  512 - 1024: port flooding groups
  1024 >    : user groups *)  

and td =
  | TDGlobal of id * ty * exp
  | TDEvent of id * event_sort * params
  | TDHandler of id * handler_sort * body 
  | TDMemop of memop
  | TDExtern of id * ty
  | TDAction of action
  | TDMain of main_handler
  (* In P4, actions and tables are "open functions". 
     they can use variables whose values are determined 
     by the environment that calls the table or action. *)
  (* The cid list should be the variables the function can modify, but is not currently used. *)
  | TDOpenFunction of id * params * statement * (cid list)



(* the main handler is a single function that contains 
   all the handlers in the Lucid program. It branches 
   on the variable "hdl_selector" to determine which 
   event handler to execute. *)
and main_handler = {
    main_id : id;
    hdl_selector : (id * ty); (* the variable containing the current event's integer identifier. note: this should be a pervasive.*)
    hdl_enum : (id * int) list; (* integer identifiers for each handler / event *)
    hdl_params : (id * params) list; (* parameters of each handler / event *)
    shared_locals : (id * ty) list; (* local variables that can be accessed from any event *)
    default_hdl : id option; (* the handler to execute for packets that arrive on non-lucid ports *)
    main_body : statement list; (* the body of the main handler. 
                                   Each statement represents the code that goes into a single stage. *)
    event_output : event_output; (* some metadata about generated events.*)
    }

and event_output = {
  (* counter variable for the number 
     of recirc / self events generated. note: this should be a pervasive. *)
  recirc_mcid_var : (id * ty);
  (* all possible sequences of events 
     that this program can generate. Used for generating an egress parser. *)
  ev_gen_seqs : (id list list);
}    

and tdecl = {td:td; tdspan: sp; tdpragma : pragma option;}
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
  | DGlobal(id, ty, exp) -> {td=TDGlobal(id, ty, exp); tdspan=decl.dspan; tdpragma = decl.dpragma;}
  | DEvent (id, es, ps) -> {td=TDEvent(id, es, ps); tdspan=decl.dspan; tdpragma = decl.dpragma;}
  | DHandler(i,s,b) -> {td=TDHandler(i,s,b); tdspan=decl.dspan; tdpragma = decl.dpragma;}
  | DMemop(m) -> {td=TDMemop(m); tdspan=decl.dspan; tdpragma = decl.dpragma;}
  | DExtern(i, t) -> {td=TDExtern(i, t); tdspan=decl.dspan; tdpragma = decl.dpragma;}
  | DAction(a) -> {td=TDAction(a); tdspan = decl.dspan; tdpragma = decl.dpragma;}
  | DParser(_) -> error "Parsers are not yet supported by the tofino backend!"
;;


(* let dbgstr_of_ids cids = List.map (Id.to_string) cids |> String.concat ", ";; *)
(* let printres title res = 
  print_endline ("---at code---"^title);
  List.iter
    (fun gen_seq -> 
      print_endline ("events generated: ");
      CorePrinting.comma_sep 
        CorePrinting.exp_to_string 
        gen_seq |> print_endline)
    res;
  print_endline ("------");
;; *)

(* append an element to each list in the sequence and add a sequence containing just the element *)
let append_and_new seqs e =
  let rec append_to_all seqs e =
    match seqs with
    | [] -> []
    | seq::seqs -> 
      (seq@[e])::(append_to_all seqs e)
  in
  match seqs with
  (* if there's nothing, make a new seq *)
  | [] -> [[e]]
  (* if there's something, append to first and recurse *)
  | seq::seqs -> 
    (seq@[e])::(append_to_all seqs e)
;;

(* find all paths of statements that match the filter_map  *)
let rec find_statement_paths paths_so_far stmt_filter stmt =
  match stmt.s with
  | SSeq(s1, s2) -> 
    let paths_including_s1 = find_statement_paths paths_so_far stmt_filter s1 in
    (* [[eva()]] *)
    (* [[eva()]] *)
    let paths_including_s2 = find_statement_paths paths_including_s1 stmt_filter s2 in
    (* printres "seq" res; *)
    paths_including_s2
  | SIf(_, s1, s2) -> 
    (* we get all paths for s1 + all paths for s2 *)
    let res = (find_statement_paths paths_so_far stmt_filter s1)
      @
      (find_statement_paths paths_so_far stmt_filter s2)
    in
    res
  | SMatch(_, ps) -> 
    let res = List.fold_left
      (fun seqs (_, bstmt) -> 
        seqs@(find_statement_paths paths_so_far stmt_filter bstmt))
      []
      ps
    in
    res
  | _ -> (
    match (stmt_filter stmt) with
    | Some r -> append_and_new paths_so_far r
    | None -> paths_so_far
  )
;;

(* find all sequences of event ids that may be generated by the program. *)
let find_generate_sequences stmt = find_statement_paths
  []
  (fun stmt -> match stmt.s with 
  | SGen(_, {e=ECall(ev_cid, _)}) -> Some(Cid.to_id ev_cid)
  | _ -> None)
  stmt
;;

let test_gen_sequencer () = 
  print_endline ("TESTING GEN SEQUENCER");
  let eva = call_sp (Cid.create ["eva"]) [] (ty TEvent) (Span.default) in
  let evb = call_sp (Cid.create ["evb"]) [] (ty TEvent) (Span.default) in
  let evc = call_sp (Cid.create ["evc"]) [] (ty TEvent) (Span.default) in
  let evd = call_sp (Cid.create ["evd"]) [] (ty TEvent) (Span.default) in
  let stmt = 
    sseq 
      (gen_sp (GSingle(None)) eva Span.default)
      (sseq 
        (sifte
          (eva)
          (gen_sp (GSingle(None)) evb Span.default)
          (gen_sp (GSingle(None)) evc Span.default))
        (gen_sp (GSingle(None)) evd Span.default))
  in
  let _ = find_generate_sequences stmt in
  ()
  (* test... *)
(*   List.iter
    (fun gen_seq -> 
      print_endline ("events generated: ");
      CorePrinting.comma_sep 
        CorePrinting.exp_to_string 
        (gen_seq |> List.split |> snd) |> print_endline)
    gen_seqs;
  print_endline ("------");
  let _ = find_ev_gen_lists stmt in 
  exit 1;
 *)
;;

(* test_gen_sequencer  ();; *)

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
      | TDHandler(hdl_id, _, (_, stmt)) -> (
        let hdl_num = match List.assoc_opt hdl_id hdl_enum with 
          | None -> error "[generate_merged_handler] could not find handler id in enum. Do events and handlers have the same internal IDs?"
          | Some hdl_num -> hdl_num
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
      | {td=TDHandler(i, s, (p, _));} -> (
        {hd with td=TDHandler(i, s, (p, snoop));}::(erase_handler_bodies tl)
      )
      | _ -> hd::(erase_handler_bodies tl)
    )
  in 
  let event_output = {recirc_mcid_var = (Id.create "recirc_mcid", (ty (TInt 16)));
    ev_gen_seqs = find_generate_sequences (List.hd main_body) |> MiscUtils.unique_list_of;} 
  in
  let tds =(erase_handler_bodies decls)
    @[{td=
    TDMain{main_id;hdl_selector;hdl_enum;hdl_params;default_hdl; main_body; shared_locals=[];event_output;}
    ;tdspan=Span.default;  tdpragma = None;}]
  in
  tds 
;;

let tdecls_of_decls decls = 
  let translated_decls = List.map tdecl_of_decl decls in
  add_main_handler translated_decls
;;


(* DEPRECIATED: generate the main handler, for a program where 
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
      | TDHandler(hdl_id, _, (_, stmt)) -> Some(stmt, hdl_id)
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
      | {td=TDHandler(i, s, (p, _));} -> (
        {hd with td=TDHandler(i, s, (p, snoop));}::(erase_handler_bodies tl)
      )
      | _ -> hd::(erase_handler_bodies tl)
    )
  in 
  let event_output = {
    recirc_mcid_var = (Id.create "recirc_mcid", (ty (TInt 16)));
    ev_gen_seqs = find_generate_sequences main_body |> MiscUtils.unique_list_of;
  } 
  in
  let tds =(erase_handler_bodies decls)
    @[{td=
    TDMain{main_id;hdl_selector;hdl_enum;hdl_params;default_hdl; main_body=[main_body]; shared_locals=[];event_output;}
    ;tdspan=Span.default; tdpragma = None;}]
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

(* returns assoc list: (arrayid : (slot width, num slots)) list *)
let array_dimensions tds =
  List.filter_map 
  (fun dec -> match dec.td with
    | TDGlobal(
        id, 
        {raw_ty=TName(ty_cid, sizes, true); _}, 
        {e=ECall(_, num_slots::_)}) -> (
        match (Cid.names ty_cid |> List.hd) with 
        | "Array" ->           
          let num_slots = InterpHelpers.int_from_exp num_slots in 
          Some((id, (List.hd sizes, num_slots)))
        | "PairArray" -> 
          let num_slots = InterpHelpers.int_from_exp num_slots in 
          Some((id, (2*(List.hd sizes), num_slots)))
        | _ -> None
    )
    | _ -> None
  )
  tds 
;;

(*** output ***)
let decl_of_tdecl tdecl = 
  match tdecl.td with
  | TDGlobal(id, ty, exp) -> decl_pragma (DGlobal(id, ty, exp)) tdecl.tdspan tdecl.tdpragma
  | TDEvent (id, es, ps) ->  {d=DEvent(id, es, ps); dspan=tdecl.tdspan; dpragma = tdecl.tdpragma;}
  | TDHandler(i, s, b) ->  {d=DHandler(i, s, b); dspan=tdecl.tdspan; dpragma = tdecl.tdpragma; }
  | TDMemop(m) ->  {d=DMemop(m); dspan=tdecl.tdspan; dpragma = tdecl.tdpragma; }
  | TDExtern(i, t) ->  {d=DExtern(i, t); dspan=tdecl.tdspan; dpragma = tdecl.tdpragma; }
  | TDAction(a) -> {d=DAction(a); dspan=tdecl.tdspan; dpragma = tdecl.tdpragma; }
  | _ -> error "[decl_of_tdecl] not a directly translatable decl"
;;

let main_to_string mainsig =  
  "// shared locals:\n"
  ^(
    (List.map
      (fun (id, ty) -> 
        (CorePrinting.ty_to_string ty)
        ^" "
        ^(CorePrinting.id_to_string id)
        ^";")
      mainsig.shared_locals)
    |> String.concat "\n")
  ^"\n// MAIN HANDLER \n"
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
  | TDOpenFunction(id, _, stmt, _) -> 
    "labeled_statement "^(CorePrinting.id_to_string id)^"{\n"
    ^CorePrinting.statement_to_string stmt
    ^"\n}"
  | _ -> CorePrinting.decl_to_string (decl_of_tdecl tdec)
;;
let tdecls_to_string tdecs = 
  List.map tdecl_to_string tdecs |> 
  String.concat "\n"
;;

let dump_prog fn tds =
  let outf = (open_out fn) in 
  Printf.fprintf outf "%s" (tdecls_to_string tds);
  flush outf
;;

(* TODO: the two functions below can make 
   coreToP4Tofino.extract_recirc_event_table more efficient *)
(* how many generates are there? *)
(* let num_plain_generates_in_program tds =
  0
;;
 *)
(* which events get used with generate_port or ports? *)
(* let port_generate_events tds = 
  []
;;
 *)

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




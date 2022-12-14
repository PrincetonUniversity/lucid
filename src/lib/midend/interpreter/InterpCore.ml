(* Interpreter evaluation functions *)
open Batteries
open CoreSyntax
open SyntaxUtils
open InterpState
module Printing = CorePrinting

let raw_integer v =
  match v.v with
  | VInt i -> i
  | _ -> error "not integer"
;;

let raw_bool v =
  match v.v with
  | VBool b -> b
  | _ -> error "not boolean"
;;

let raw_event v =
  match v.v with
  | VEvent e -> e
  | _ -> error "not event"
;;

let raw_group v =
  match v.v with
  | VGroup ls -> ls
  | _ -> error "not group"
;;

let interp_op op vs =
  let vs = List.map extract_ival vs in
  match op, vs with
  | And, [v1; v2] -> vbool (raw_bool v1 && raw_bool v2)
  | Or, [v1; v2] -> vbool (raw_bool v1 || raw_bool v2)
  | Not, [v] -> vbool (not (raw_bool v))
  | Neg, [_] -> failwith "Not actually supported since all ints are unsigned"
  | Cast size, [v] -> vinteger (Integer.set_size size (raw_integer v))
  | Eq, [v1; v2] -> vbool (v1.v = v2.v)
  | Neq, [v1; v2] ->
    vbool (not (Integer.equal (raw_integer v1) (raw_integer v2)))
  | Less, [v1; v2] -> vbool (Integer.lt (raw_integer v1) (raw_integer v2))
  | More, [v1; v2] -> vbool (Integer.lt (raw_integer v2) (raw_integer v1))
  | Leq, [v1; v2] -> vbool (Integer.leq (raw_integer v1) (raw_integer v2))
  | Geq, [v1; v2] -> vbool (Integer.geq (raw_integer v1) (raw_integer v2))
  | Plus, [v1; v2] -> vinteger (Integer.add (raw_integer v1) (raw_integer v2))
  | SatPlus, [v1; v2] ->
    let res = Integer.add (raw_integer v1) (raw_integer v2) in
    if Integer.lt res (raw_integer v1)
    then
      vinteger
        (Integer.create ~value:(-1) ~size:(Integer.size (raw_integer v1)))
    else vinteger res
  | Sub, [v1; v2] -> vinteger (Integer.sub (raw_integer v1) (raw_integer v2))
  | SatSub, [v1; v2] ->
    if Integer.lt (raw_integer v1) (raw_integer v2)
    then
      vinteger (Integer.create ~value:0 ~size:(Integer.size (raw_integer v1)))
    else vinteger (Integer.sub (raw_integer v1) (raw_integer v2))
  | Conc, [v1; v2] ->
    let v1, v2 = raw_integer v1, raw_integer v2 in
    vinteger (Integer.concat v1 v2)
  | BitAnd, [v1; v2] ->
    vinteger (Integer.bitand (raw_integer v1) (raw_integer v2))
  | BitOr, [v1; v2] ->
    vinteger (Integer.bitor (raw_integer v1) (raw_integer v2))
  | BitXor, [v1; v2] ->
    vinteger (Integer.bitxor (raw_integer v1) (raw_integer v2))
  | BitNot, [v1] -> vinteger (Integer.bitnot (raw_integer v1))
  | LShift, [v1; v2] ->
    vinteger
      (Integer.shift_left (raw_integer v1) (raw_integer v2 |> Integer.to_int))
  | RShift, [v1; v2] ->
    vinteger
      (Integer.shift_right (raw_integer v1) (raw_integer v2 |> Integer.to_int))
  | Slice (hi, lo), [v] ->
    vinteger
      (Integer.shift_right (raw_integer v) lo |> Integer.set_size (hi - lo + 1))
  | ( ( Not
      | Neg
      | BitNot
      | And
      | Or
      | Eq
      | Neq
      | Less
      | More
      | Leq
      | Geq
      | Plus
      | Sub
      | SatPlus
      | SatSub
      | BitAnd
      | BitOr
      | BitXor
      | LShift
      | RShift
      | Conc
      | Cast _
      | Slice _ )
    , _ ) ->
    error
      ("bad operator: "
      ^ CorePrinting.op_to_string op
      ^ " with "
      ^ string_of_int (List.length vs)
      ^ " arguments")
;;

let lookup_var swid nst locals cid =
  try Env.find cid locals with
  | _ -> State.lookup swid cid nst
;;

let interp_eval exp : State.ival =
  match exp.e with
  | EVal v -> V v
  | _ -> error ("[interp_eval] expected a value expression, but got something else.")
;;
let rec interp_exp (nst : State.network_state) swid locals e : State.ival =
  (* print_endline @@ "Interping: " ^ CorePrinting.exp_to_string e; *)
  let interp_exps = interp_exps nst swid locals in
  let lookup cid = lookup_var swid nst locals cid in
  match e.e with
  | EVal v -> V v
  | EVar cid -> lookup cid
  | EOp (op, es) ->
    let vs = interp_exps es in
    V (interp_op op vs)
  | ECall (cid, es) ->
    let vs = interp_exps es in
    (match lookup cid with
    | V _ ->
      error
        (Cid.to_string cid
        ^ " is a value identifier and cannot be used in a call")
    | F f -> V (f nst swid vs))
  | EHash (size, args) ->
    let vs = interp_exps args in
    let vs =
      List.map
        (function
          | State.V v -> v.v
          | _ -> failwith "What? No hashing functions!")
        vs
    in
    let extract_int = function
      | VInt n -> n
      | _ -> failwith "No good"
    in
    (match vs with
    | VInt seed :: tl ->
      (* Special case: if the hash seed is 1 and all the arguments are integers,
          we perform an identity hash (i.e. just concatenate the arguments) *)
      if Z.to_int (Integer.value seed) = 1
      then (
        try
          let n =
            List.fold_left
              (fun acc v -> Integer.concat acc (extract_int v))
              (List.hd tl |> extract_int)
              (List.tl tl)
          in
          V (VInt (Integer.set_size size n) |> value)
        with
        | Failure _ ->
          (* Fallback to non-special case *)
          let hashed = Legacy.Hashtbl.seeded_hash (Integer.to_int seed) tl in
          V (vint hashed size))
      else (
        (* For some reason hash would only take into account the first few elements
           of the list, so this forces all of them to have some impact on the output *)
        let feld = List.fold_left (fun acc v -> Hashtbl.hash (acc, v)) 0 tl in
        let hashed = Legacy.Hashtbl.seeded_hash (Integer.to_int seed) feld in
        V (vint hashed size))
    | _ -> failwith "Wrong arguments to hash operation")
  | EFlood e1 ->
    let port =
      interp_exp nst swid locals e1
      |> extract_ival
      |> raw_integer
      |> Integer.to_int
    in
    V (vgroup [-(port + 1)])
  | ETableCreate _ -> (
    error "[InterpCore.interp_exp] got a table_create expression, which should not happen\
    because the table creation should be interpeter in the declaration";
  )

and interp_exps nst swid locals es : State.ival list =
  List.map (interp_exp nst swid locals) es
;;


let bitmatch bits n =
  let open Z in
  let bits = List.rev bits in
  let two = Z.of_int 2 in
  let rec aux bits n =
    match bits with
    | [] -> n = zero
    | hd :: tl ->
      aux tl (shift_right n 1)
      &&
      (match hd with
      | 0 -> rem n two = zero
      | 1 -> rem n two <> zero
      | _ -> true)
  in
  aux bits n
;;

let matches_pat vs ps =
  if ps = [PWild]
  then true
  else
    List.for_all2
      (fun v p ->
        let v = v.v in
        match p, v with
        | PWild, _ -> true
        | PNum pn, VInt n -> Z.equal (Integer.value n) pn
        | PBit bits, VInt n -> bitmatch bits (Integer.value n)
        | _ -> false)
      vs
      ps
;;

let printf_replace vs (s : string) : string =
  List.fold_left
    (fun s v ->
      match v.v with
      | VBool b -> snd @@ String.replace s "%b" (string_of_bool b)
      | VInt n -> snd @@ String.replace s "%d" (Z.to_string (Integer.value n))
      | _ -> error "Cannot print values of this type")
    s
    vs
;;

(* print an exit event as a json to stdout *)
let output_exit_event swid port_opt event time = 
  let open Yojson.Basic in 
  let raw_json_val v = 
    match v.v with 
    | VInt i -> `Int (Integer.to_int i)
    | VBool b -> `Bool b
    | _ -> error "not an int or bool"
  in 
  let {eid; data; _} = event in 
  let name = `String (CorePrinting.cid_to_string eid) in 
  let args = `List (List.map raw_json_val data) in 
  let port = match port_opt with
    | None -> -1
    | Some p -> p
  in 
  let locs = `List [`String (Printf.sprintf "%i:%i" swid port)] in 
  let timestamp = `Int time in 
  (* let args = CorePrinting.value_to_string data in  *)
  let evjson = `Assoc [
    ("name", name);
    ("args", args);
    ("locations", locs);
    ("timestamp", timestamp)
    ]
  in 
  print_endline (Yojson.Basic.to_string evjson);
;;


(* print event as json if interactive mode is set, 
   else log for final report *)
let log_exit swid port_opt event (nst : State.network_state) = 
  if (Cmdline.cfg.interactive) 
  then 
    (output_exit_event swid port_opt event nst.current_time)
  else 
  (State.log_exit swid port_opt event nst)
;; 



let partial_interp_const_action_args nst swid env eargs =
  List.map
    (fun exp -> match (interp_exp nst swid env exp) with 
      | V v -> {e=EVal(v); espan=Span.default; ety=v.vty}
      | _ -> error "[interp_dtable] default argument evaluated to function pointer, expected value")
    eargs
;; 


let rec interp_statement nst swid locals s =
  (* (match s.s with
  | SSeq _ | SNoop -> () (* We'll print the sub-parts when we get to them *)
  | _ -> print_endline @@ "Interpreting " ^ CorePrinting.stmt_to_string s); *)
  let interpret_exp = interp_exp nst swid in 
  let interp_exp = interp_exp nst swid locals in
  let interp_s = interp_statement nst swid locals in
  match s.s with
  | SNoop -> locals
  | SAssign (id, e) ->
    if not (Env.mem (Id id) locals)
    then
      if State.mem_env swid (Id id) nst
      then
        error
          (Printf.sprintf
             "Variable %s is global and cannot be assigned to"
             (Id.name id))
      else error (Printf.sprintf "Unbound variable %s" (Id.name id));
    Env.add (Id id) (interp_exp e) locals
  | SLocal (id, _, e) -> Env.add (Id id) (interp_exp e) locals
  | SPrintf (s, es) ->
    let vs = List.map (fun e -> interp_exp e |> extract_ival) es in
    if (Cmdline.cfg.interactive)
    (* for interactive mode, print messages to stderr *)
    then (prerr_endline (printf_replace vs s)) 
    else (print_endline (printf_replace vs s));
    locals
  | SIf (e, ss1, ss2) ->
    let b = interp_exp e |> extract_ival |> raw_bool in
    if b then interp_s ss1 else interp_s ss2
  | SSeq (ss1, ss2) ->
    let locals = interp_s ss1 in
    (* Stop evaluating after hitting a return statement *)
    if !(nst.switches.(swid).retval) <> None
    then locals
    else interp_statement nst swid locals ss2
  | SGen (g, e) ->
    (* TODO: Right now, port numbers for generate_single are
       arbitrary (always 0). We could do better by e.g. finding a path through
       the graph and figuring out which port number it ends at *)
    let locs =
      match g with
      | GSingle None ->
        [ ( swid
          , State.lookup swid (Cid.from_string "recirculation_port") nst
            |> extract_ival
            |> raw_integer
            |> Integer.to_int ) ]
      | GSingle (Some e) ->
        [interp_exp e |> extract_ival |> raw_integer |> Integer.to_int, 0]
      | GMulti grp ->
        let ports = interp_exp grp |> extract_ival |> raw_group in
        (match ports with
        | [port] when port < 0 ->
          (* Flooding: send to every connected switch *)
          (-1, port)
          :: (IntMap.find swid nst.links
             |> IntMap.bindings
             |> List.filter_map (fun (p, dst) ->
                    if p = -(port + 1) then None else Some dst))
        | _ -> List.map (fun port -> State.lookup_dst nst (swid, port)) ports)
      | GPort port ->
        let port =
          interp_exp port |> extract_ival |> raw_integer |> Integer.to_int
        in
        [State.lookup_dst nst (swid, port)]
    in
    let event = interp_exp e |> extract_ival |> raw_event in
    if Env.find event.eid nst.event_sorts = EExit
    then log_exit swid None event nst
    else
      List.iter
        (fun (dst_id, port) ->
          if dst_id = -1 (* lookup_dst failed *)
          then (
            log_exit swid (Some port) event nst;
          )
          else State.push_event dst_id port event nst)
        locs;
    locals
  | SRet (Some e) ->
    let v = interp_exp e |> extract_ival in
    (* Computation stops if retval is Some *)
    nst.switches.(swid).retval := Some v;
    locals
  | SRet None ->
    (* Return a dummy value; type system guarantees it won't be used *)
    nst.switches.(swid).retval := Some (vint 0 0);
    locals
  | SUnit e ->
    ignore (interp_exp e);
    locals
  | SMatch (es, bs) ->
    let vs = List.map (fun e -> interp_exp e |> extract_ival) es in
    let first_match =
      try List.find (fun (pats, _) -> matches_pat vs pats) bs with
      | _ -> error "Match statement did not match any branch!"
    in
    interp_s (snd first_match)
  | STableInstall(tbl_id, entries) -> 
    (* install entries into the pipeline *)
    (* for each entry: 
        1. evaluate arguments to install
        2. need pipeline entry install or install next?
    *)
    (* evaluate all entry action args *)
    let entries = List.map 
      (fun entry -> 
        let eargs = partial_interp_const_action_args nst swid locals entry.eargs in
        {entry with eargs})
      entries
    in
    (* get index of table in pipeline *)
    let tbl_pos = match (lookup_var swid nst locals (Cid.id tbl_id)) with
      | V {v = VGlobal stage} -> stage
      | _ -> error "Table did not evaluate to a pipeline object reference"
    in
    (* call install_table_entry for each entry *)
    List.iter (State.install_table_entry_switch swid tbl_pos nst) entries;
    (* return unmodified locals context *)
    locals
  | STableMatch(tm) -> 
    (* load the dynamic entries from the pipeline *)
    let tbl_pos = match (interp_exp tm.tbl) with
      | V {v = VGlobal stage} -> stage
      | _ -> error "Table did not evaluate to a pipeline object reference"
    in
    let default, entries = State.get_table_entries_switch swid tbl_pos nst in 
    (* find the first matching case *)
    let key_vs = List.map (fun e -> interp_exp e |> extract_ival) tm.keys in    
    let fst_match = List.fold_left 
      (fun fst_match entry -> 
        match fst_match with 
        | None -> 
          if (matches_pat key_vs entry.ematch)
          then Some(entry.eaction, entry.eargs)
          else None
        | Some(_) -> fst_match)
      None
      entries
    in 
    (* if there's no matching entry, use the default action. *)
    let acnid, e_const_args = match fst_match with
      | Some(acnid, const_args) -> acnid, const_args
      | None -> default
    in     
    (* find the action in context *)
    let action = State.lookup_action (Id acnid) nst in
    (* extract values from const arguments *)
    let const_args = List.map interp_eval e_const_args in 
    (* evaluate the runtime action arguments *)   
    let dyn_args = List.map interp_exp tm.args in
    (* bind install- and match-time parameters in env *)
    let inner_locals = List.fold_left2
      (fun inner_locals v (id, _) -> 
        Env.add (Id id) v inner_locals)
      locals
      (const_args@dyn_args)
      (action.aconst_params@action.aparams)
    in
    (* evaluate the action's expressions *)
    let acn_ret_vals = List.map (interpret_exp inner_locals) action.abody in
    (* update variables set by table's output in the env *)
    let locals = List.fold_left2
      (fun locals v id -> 
        Env.add (Id id) v locals)
      locals
      acn_ret_vals
      tm.outs
    in 
    locals
;;


let interp_dtable (nst : State.network_state) swid id ty e =
  (* FIXME: hacked in a dtable interp that gets called from 
            interp_dglobal. *)

  let st = nst.switches.(swid) in
  let p = st.pipeline in
  let idx = Pipeline.length p in

  (* add element to pipeline *)
  let new_p = match ty.raw_ty with 
    | TTable(_) -> (
      match e.e with
      | ETableCreate(t) -> 
        (* eval args to value expressions *)
        let def_acn_args = partial_interp_const_action_args nst swid (Env.empty) (snd t.tdefault) in
        (* construct the default entry with wildcard args *)
(*         let def_entry_pats = List.map (fun _ -> PWild) (tbl_ty.tkey_sizes) in
        let (def_entry:tbl_entry) = {ematch=def_entry_pats; eaction=(Cid.to_id (fst t.tdefault)); eargs=def_install_args;eprio=0;} in *)
        Pipeline.append p (Pipeline.mk_table id t.tsize (fst t.tdefault |> Cid.to_id, def_acn_args))
      | _ -> error "[interp_dtable] incorrect constructor for table")
    | _ -> error "[interp_dtable] called to create a non table type object"
  in
  nst.switches.(swid) <- { st with pipeline = new_p };
  State.add_global swid (Id id) (V (vglobal idx ty)) nst;
  nst
;;

let _interp_dglobal (nst : State.network_state) swid id ty e =
  (* FIXME: This functions is probably more complicated than it needs to be.
     We can probably do this a lot better by writing the Array.create function
     in Arrays.ml (and similarly for counters), then just calling that. But I
     don't want to muck around with the interpreter for now, so I'm sticking to
     quick fixes. *)
  let st = nst.switches.(swid) in
  let p = st.pipeline in
  let idx = Pipeline.length p in
  let gty_name, gty_sizes =
    match ty.raw_ty with
    | TName (cid, sizes, _) -> Cid.names cid, sizes
    | _ -> failwith "Bad DGlobal"
  in
  let args =
    match e.e with
    | ECall (_, args) -> args
    | _ -> failwith "Bad constructor"
  in
  let new_p =
    match gty_name, gty_sizes, args with
    | ["Array"; "t"], [size], [e] ->
      let len =
        interp_exp nst swid Env.empty e
        |> extract_ival
        |> raw_integer
        |> Integer.to_int
      in
      Pipeline.append p (Pipeline.mk_array id size len false)
    | ["Counter"; "t"], [size], [e] ->
      let init_value =
        interp_exp nst swid Env.empty e |> extract_ival |> raw_integer
      in
      let new_p = Pipeline.append p (Pipeline.mk_array id size 1 false) in
      ignore
        (Pipeline.update
           ~stage:idx
           ~idx:0
           ~getop:(fun _ -> Z.zero)
           ~setop:(fun _ -> init_value)
           new_p);
      new_p
    | ["PairArray"; "t"], [size], [e] ->
      let len =
        interp_exp nst swid Env.empty e
        |> extract_ival
        |> raw_integer
        |> Integer.to_int
      in
      Pipeline.append p (Pipeline.mk_array id size len true)
    | _ ->
      error
        "Wrong number of arguments to global constructor, or user type \
         appeared during interpretation"
  in
  nst.switches.(swid) <- { st with pipeline = new_p };
  State.add_global swid (Id id) (V (vglobal idx ty)) nst;
  nst
;;

let interp_dglobal (nst : State.network_state) swid id ty e =
  match ty.raw_ty with 
    | TTable _ -> interp_dtable nst swid id ty e
    | _ -> _interp_dglobal nst swid id ty e


let interp_complex_body params body nst swid args =
  let args, default = List.takedrop (List.length params) args in
  let default = List.hd default in
  let cell1_val = List.hd args in
  let cell2_val =
    match args with
    | [_; v; _; _] -> v
    | _ -> default
  in
  let ret_id = Id.create "memop_retval" in
  let locals =
    List.fold_left2
      (fun acc arg (id, _) -> Env.add (Id id) arg acc)
      Env.empty
      args
      params
    |> Env.add (Id Builtins.cell1_id) cell1_val
    |> Env.add (Id Builtins.cell2_id) cell2_val
    |> Env.add (Id ret_id) default
  in
  let interp_b locals = function
    | None -> locals
    | Some (id, e) -> Env.add (Id id) (interp_exp nst swid locals e) locals
  in
  let interp_cro id locals = function
    | None -> false, locals
    | Some (e1, e2) ->
      let b = interp_exp nst swid locals e1 |> extract_ival |> raw_bool in
      if b
      then b, Env.add (Id id) (interp_exp nst swid locals e2) locals
      else b, locals
  in
  let interp_cell id locals (cro1, cro2) =
    let b, locals = interp_cro id locals cro1 in
    if b then locals else snd @@ interp_cro id locals cro2
  in
  let locals = interp_b locals body.b1 in
  let locals = interp_b locals body.b2 in
  let locals = interp_cell Builtins.cell1_id locals body.cell1 in
  let locals = interp_cell Builtins.cell2_id locals body.cell2 in
  List.iter
    (fun (cid, es) ->
      ignore
      @@ interp_exp nst swid locals (call_sp cid es (ty TBool) Span.default))
    body.extern_calls;
  let _, locals = interp_cro ret_id locals body.ret in
  let vs =
    [Builtins.cell1_id; Builtins.cell2_id; ret_id]
    |> List.map (fun id -> (Env.find (Id id) locals |> extract_ival).v)
  in
  { v = VTuple vs; vty = ty TBool (* Dummy type *); vspan = Span.default }
;;

let interp_memop params body nst swid args =
  (* Memops are polymorphic, but since the midend doesn't understand polymorphism,
      the size of all the ints in its body got set to 32. We'll just handle this by
      going through now and setting all the sizes to that of the first argument.
      It only actually matters for integer constants. *)
  let replacer =
    object
      inherit [_] s_map
      method! visit_VInt sz n = VInt (Integer.set_size sz n)
    end
  in
  let sz = List.hd args |> extract_ival |> raw_integer |> Integer.size in
  let body = replacer#visit_memop_body sz body in
  match body with
  | MBComplex body -> interp_complex_body params body nst swid args
  | MBReturn e ->
    let locals =
      List.fold_left2
        (fun acc arg (id, _) -> Env.add (Id id) arg acc)
        Env.empty
        args
        params
    in
    interp_exp nst swid locals e |> extract_ival
  | MBIf (e1, e2, e3) ->
    let locals =
      List.fold_left2
        (fun acc arg (id, _) -> Env.add (Id id) arg acc)
        Env.empty
        args
        params
    in
    let b = interp_exp nst swid locals e1 |> extract_ival |> raw_bool in
    if b
    then interp_exp nst swid locals e2 |> extract_ival
    else interp_exp nst swid locals e3 |> extract_ival
;;

let interp_decl (nst : State.network_state) swid d =
  (* print_endline @@ "Interping decl: " ^ Printing.decl_to_string d; *)
  match d.d with
  | DGlobal (id, ty, e) -> interp_dglobal nst swid id ty e
  | DAction (acn) -> 
    (* add the action to the environment *)
    State.add_action (Cid.id acn.aid) acn nst
  | DHandler (id, (params, body)) ->
    let f nst swid port event =
      let builtin_env =
        List.fold_left
          (fun acc (k, v) -> Env.add k v acc)
          Env.empty
          [ Id Builtins.this_id, State.V (vevent { event with edelay = 0 })
          ; Id Builtins.ingr_port_id, State.V (vint port 32) ]
      in
      let locals =
        List.fold_left2
          (fun acc v (id, _) -> Env.add (Id id) (State.V v) acc)
          builtin_env
          event.data
          params
      in
      State.update_counter swid event nst;
      Pipeline.reset_stage nst.switches.(swid).pipeline;
      ignore @@ interp_statement nst swid locals body
    in
    State.add_handler (Cid.id id) f nst
  | DEvent (id, _, _) ->
    let f _ _ args =
      vevent { eid = Id id; data = List.map extract_ival args; edelay = 0 }
    in
    State.add_global swid (Id id) (State.F f) nst;
    nst
  | DMemop (id, params, body) ->
    let f = interp_memop params body in
    State.add_global swid (Cid.id id) (State.F f) nst;
    nst
  | DExtern _ ->
    failwith "Extern declarations should be handled during preprocessing"
;;

let process_decls nst ds =
  let rec aux i (nst : State.network_state) =
    if i = Array.length nst.switches
    then nst
    else aux (i + 1) (List.fold_left (fun nst -> interp_decl nst i) nst ds)
  in
  aux 0 nst
;;

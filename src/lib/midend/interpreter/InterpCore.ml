(* Interpreter evaluation functions *)
open Batteries
open Yojson.Basic
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
  let extract_int = function
    | VInt n -> n
    | _ -> failwith "No good"
  in
  let vs = List.map extract_ival vs in
  match op, vs with
  | And, [v1; v2] -> vbool (raw_bool v1 && raw_bool v2)
  | Or, [v1; v2] -> vbool (raw_bool v1 || raw_bool v2)
  | Not, [v] -> vbool (not (raw_bool v))
  | Neg, [v1] ->
    (* Compute 0 - v1 *)
    let v1 = raw_integer v1 in
    vinteger (Integer.sub (Integer.create ~value:0 ~size:(Integer.size v1)) v1)
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
  | PatExact, [v] ->
    let vint = extract_int v.v in
    let pat_len = Integer.size vint in
    let pat_val = Integer.to_int vint in
    let bs = int_to_bitpat pat_val pat_len in
    let outv = vpat bs in
    (*     print_endline ("[interp_op.PatExact] input: "
      ^(CorePrinting.value_to_string v)
      ^" output: "^(CorePrinting.value_to_string outv)); *)
    outv
  | PatMask, [v; m] ->
    let vint = extract_int v.v in
    let pat_len = Integer.size vint in
    let pat_val = Integer.to_int vint in
    let pat_mask = Integer.to_int (extract_int m.v) in
    let bs = int_mask_to_bitpat pat_val pat_mask pat_len in
    let outv = vpat bs in
    (*     print_endline ("[interp_op.PatMask] input: "
      ^(CorePrinting.value_to_string v)
      ^" output: "^(CorePrinting.value_to_string outv)); *)
    outv
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
      | Slice _
      | PatExact
      | PatMask )
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


let port_arg locals = 
  let (port:CoreSyntax.value) = match Env.find (Id Builtins.ingr_port_id) locals with 
    | InterpSyntax.V(port_val) -> port_val
    | _ -> error "could not find input port while interpreting parser!"
  in
  port
;;

let get_payload payload_id locals = 
  match Env.find (Id payload_id) locals with 
  | InterpSyntax.V(payload) -> CoreSyntax.vpat_to_payload payload
  | _ -> error "could not find current packet buffer while interpreting parser!"
;;

let update_payload payload_id payload locals = 
  let payload = CoreSyntax.payload_to_vpat payload in
  Env.add (Id payload_id) (InterpSyntax.V(payload)) (Env.remove (Id payload_id) locals)
;;




let interp_eval exp : 'a InterpSyntax.ival =
  match exp.e with
  | EVal v -> V v
  | _ ->
    error "[interp_eval] expected a value expression, but got something else."
;;

let rec interp_exp (nst : State.network_state) swid locals e : 'a InterpSyntax.ival =
  let interp_exps = interp_exps nst swid locals in
  let lookup cid = lookup_var swid nst locals cid in
  match e.e with
  | EVal v -> V v
  | EVar cid -> 
    let res = lookup cid in
    res
  | EOp (op, es) ->
    let vs = interp_exps es in
    V (interp_op op vs)
  | ECall (cid, es) ->
    let vs = interp_exps es in
    (match lookup cid with
     | P _ 
     | V _ ->
       error
         (Cid.to_string cid
         ^ " is a value identifier and cannot be used in a call")
     | F f -> V (f nst swid vs)
   )
  | EHash (size, args) ->
    let vs = interp_exps args in
    let vs =
      List.map
        (function
         | InterpSyntax.V v -> v.v
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
  | ETableCreate _ ->
    error
      "[InterpCore.interp_exp] got a table_create expression, which should not \
       happenbecause the table creation should be interpeter in the \
       declaration"

and interp_exps nst swid locals es : 'a InterpSyntax.ival list =
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

(* for tables (matches_pat is for match statements) *)
let matches_pat_vals (vs : value list) (pats : value list) =
  List.for_all2
    (fun v p ->
      match v.v, p.v with
      | VInt n, VPat bs -> bitmatch bs (Integer.value n)
      | _ -> false)
    vs
    pats
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

(*
{
  interp_msg : {
    "type" : "printf"
    "value" : ...
    "location"
  }


}
 *)

(* print message to a json record *)
let interp_report msgty msg swid_opt =
  (if Cmdline.cfg.json || Cmdline.cfg.interactive
  then
    `Assoc
      ([msgty, `String msg]
      (* `Assoc ["interpreter_message", `Assoc ["inter_message", `String msgty; "value", `String msg]] *)
      @
      match swid_opt with
      | Some swid -> ["switch", `Int swid]
      | _ -> [])
    |> Yojson.Basic.pretty_to_string
  else msg)
  |> print_endline
;;

let print_event_arrival swid str = interp_report "event_arrival" str (Some swid)
let print_final_state str = interp_report "final_state" str None
let print_printf swid str = interp_report "printf" str (Some swid)

(* print an exit event as a json to stdout *)
let print_exit_event swid port_opt (event:InterpSyntax.internal_event_val) time =
  let base_out_tups = match event with 
    | IEvent(v) -> InterpSyntax.event_val_to_json v
    | IPacket(p) -> InterpSyntax.packet_event_to_json p
    | IControl _ -> error "attempting to send a control event out of a network port"
  in
  (* add location and timestamp metadata *)
  let port =
    match port_opt with
    | None -> -1
    | Some p -> p
  in
  let locs = `List [`String (Printf.sprintf "%i:%i" swid port)] in
  let timestamp = `Int time in
  let evjson =
    `Assoc
      (base_out_tups@["locations", locs; "timestamp", timestamp])
  in
  print_endline (Yojson.Basic.to_string evjson)
;;

(* print event as json if interactive mode is set,
   else log for final report *)
let log_exit swid port_opt (event:InterpSyntax.internal_event_val) (nst : State.network_state) =
  if Cmdline.cfg.interactive
  then print_exit_event swid port_opt event nst.current_time
  else State.log_exit swid port_opt event nst
;;

let partial_interp_exps nst swid env exps =
  List.map
    (fun exp ->
      match interp_exp nst swid env exp with
      | V v -> { e = EVal v; espan = Span.default; ety = v.vty }
      | _ ->
        error
          "[partial_interp_exps] default argument evaluated to function \
           pointer, expected value")
    exps
;;

let rec interp_statement nst hdl_sort swid locals s =
  (* (match s.s with
  | SSeq _ | SNoop -> () (* We'll print the sub-parts when we get to them *)
  | _ -> print_endline @@ "Interpreting " ^ CorePrinting.stmt_to_string s); *)
  let interpret_exp = interp_exp nst swid in
  let interp_exp = interp_exp nst swid locals in
  let interp_s = interp_statement nst hdl_sort swid locals in
  match s.s with
  | SNoop -> locals
  | SAssign (id, e) ->
    if not (Env.mem id locals)
    then
      if State.mem_env swid (id) nst
      then
        error
          (Printf.sprintf
             "Variable %s is global and cannot be assigned to"
             (Cid.to_id id |> Id.name))
      else error (Printf.sprintf "Unbound variable %s" (Id.name (Cid.to_id id)));
    Env.add (id) (interp_exp e) locals
  | SLocal (id, _, e) -> Env.add (Id id) (interp_exp e) locals
  | SPrintf (s, es) ->
    let vs = List.map (fun e -> interp_exp e |> extract_ival) es in
    let strout = printf_replace vs s in
    print_printf swid strout;
    locals
  | SIf (e, ss1, ss2) ->
    let b = interp_exp e |> extract_ival |> raw_bool in
    if b then interp_s ss1 else interp_s ss2
  | SSeq (ss1, ss2) ->
    let locals = interp_s ss1 in
    (* Stop evaluating after hitting a return statement *)
    if !(nst.switches.(swid).retval) <> None
    then locals
    else interp_statement nst hdl_sort swid locals ss2

  | SGen (g, e) -> (


    (* first, get the event value, without any payload *)
    let event = interp_exp e |> extract_ival |> raw_event in
    let ev_sort = Env.find event.eid nst.event_sorts in
    (* now, we process differently depending on whether it is a packet event 
       or background event. Packet events get serialized into packets, 
       background events do not. *)
    (* serialize packet events *)
    let event_val = match ev_sort, hdl_sort with 
      | EBackground, _ -> InterpSyntax.IEvent(event) (* background events stay as events *)
      | EPacket, HEgress -> (* packet events get serialized into a packet, which is just a 
         bitstring with a location. The payload is included via arguments. *)
        let pkt = InterpPayload.serialize_packet_event event in
        InterpSyntax.IPacket {pkt_val=pkt; pkt_edelay=event.edelay}
      | EPacket, _ -> InterpSyntax.IEvent(event) (* ingress packet events stay as events *)
    in
    (* let ingress_next_locs = match g with *)

    (* left off here. Right now, all the processing is in ingress and it sends it directly to the next ingress. 
       However, output_ports are _OUR_ output ports. What we need to do next is send the event to 
       the egress queue with that port. Then, in a separate step, dequeue from the egress port and do 
       another lookup to send it to the final destination. *)
    if (hdl_sort = HData) then (
      (* ingress case *)
      let output_ports = match g with
        | GSingle None ->
          [ State.Port(State.lookup swid (Cid.from_string "recirculation_port") nst
          |> extract_ival
          |> raw_integer
          |> Integer.to_int )  ]
        | GSingle (Some exp) -> 
          let swid = interp_exp exp |> extract_ival |> raw_integer |> Integer.to_int in
          [(State.Switch(swid))]
        | GPort port -> [State.Port(interp_exp port |> extract_ival |> raw_integer |> Integer.to_int)]
        | GMulti grp -> 
          let ports = interp_exp grp |> extract_ival |> raw_group in
          (match ports with
          | [port] when port < 0 ->
            (* flooding *)
            let tl = (IntMap.find swid nst.links (*get a tuple list of (local_port, (remote_switch, remote_port))*)
              |> IntMap.bindings
              |> List.filter_map (fun (p, (dst, _)) ->
                  (* if the port in the bindings (p) is equal to the negation of the port argument + 1, 
                      then send to this output port. We have to be careful: do not want to flood to recirculation port! *)
                  if ((p = -(port + 1)) || (dst = swid)) then None else Some (  
                  State.Port(p)))) in
            (* this is just here for logging... switch -1 at port with number of the group.  *)
            let hd = State.PExit(port) in
            hd::tl
              
          | _ -> 
            List.map (fun port -> State.Port(port)) ports)
      in
      (* push all the events to output ports *)
      List.iter (fun out_port -> 
        State.ingress_send swid out_port event_val nst) 
        output_ports;
      locals 
    )
    else (
      let extract_int = function
        | VInt n -> n
        | _ -> failwith "No good"
      in      
      (* egress case -- always just push the event to the other side of the port *)
      let port = ((port_arg locals).v |> extract_int ).value |> Z.to_int in 
      State.egress_send swid port event_val nst;
      locals
    ) 
  )
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
  | STableInstall (tbl, entries) ->
    (* install entries into the pipeline *)
    (* evaluate entry patterns and install-time action args to EVals *)
    let entries =
      List.map
        (fun entry ->
          let ematch = partial_interp_exps nst swid locals entry.ematch in
          let eargs = partial_interp_exps nst swid locals entry.eargs in
          { entry with ematch; eargs })
        entries
    in
    (* get index of table in pipeline *)
    let tbl_pos =
      match interp_exp tbl with
      | V { v = VGlobal stage } -> stage
      | _ -> error "Table did not evaluate to a pipeline object reference"
    in
    (* call install_table_entry for each entry *)
    List.iter (fun entry -> InterpSwitch.install_table_entry tbl_pos entry (State.sw nst swid)) entries;
    (* return unmodified locals context *)
    locals
  | STableMatch tm ->
    (* load the dynamic entries from the pipeline *)
    let tbl_pos =
      match interp_exp tm.tbl with
      | V { v = VGlobal stage } -> stage
      | _ -> error "Table did not evaluate to a pipeline object reference"
    in
    let default, entries = InterpSwitch.get_table_entries tbl_pos (State.sw nst swid) in
    (* find the first matching case *)
    let key_vs = List.map (fun e -> interp_exp e |> extract_ival) tm.keys in
    let fst_match =
      List.fold_left
        (fun fst_match entry ->
          (* print_endline ("checking entry: "^(CorePrinting.entry_to_string entry)); *)
          match fst_match with
          | None ->
            let pat_vs =
              List.map (fun e -> interp_exp e |> extract_ival) entry.ematch
            in
            if matches_pat_vals key_vs pat_vs
            then Some (entry.eaction, entry.eargs)
            else None
          | Some _ -> fst_match)
        None
        entries
    in
    (* if there's no matching entry, use the default action. *)
    let acnid, e_const_args =
      match fst_match with
      | Some (acnid, const_args) -> acnid, const_args
      | None -> default
    in
    (* find the action in context *)
    let action = State.lookup_action (Id acnid) nst in
    (* extract values from const arguments *)
    let const_args = List.map interp_eval e_const_args in
    (* evaluate the runtime action arguments *)
    let dyn_args = List.map interp_exp tm.args in
    (* bind install- and match-time parameters in env *)
    let inner_locals =
      List.fold_left2
        (fun inner_locals v (id, _) -> Env.add (Id id) v inner_locals)
        locals
        (const_args @ dyn_args)
        (action.aconst_params @ action.aparams)
    in
    (* evaluate the action's expressions *)
    let acn_ret_vals = List.map (interpret_exp inner_locals) action.abody in
    (* update variables set by table's output in the env *)
    let locals =
      List.fold_left2
        (fun locals v id -> Env.add (Id id) v locals)
        locals
        acn_ret_vals
        tm.outs
    in
    locals
;;

let interp_dtable (nst : State.network_state) swid id ty e =
  (* FIXME: hacked in a dtable interp that gets called from
            interp_dglobal. *)
  let extract_int exp =
    let size_val = interp_exp (nst : State.network_state) swid Env.empty exp in
    match size_val with
    | V { v = VInt n } -> Integer.to_int n
    | _ -> failwith "not an int val"
  in
  let st = nst.switches.(swid) in
  let p = st.pipeline in
  let idx = Pipeline.length p in
  (* add element to pipeline *)
  let new_p =
    match ty.raw_ty with
    | TTable _ ->
      (match e.e with
       | ETableCreate t ->
         (* eval args to value expressions *)
         let def_acn_args =
           partial_interp_exps nst swid Env.empty (snd t.tdefault)
         in
         (* construct the default entry with wildcard args *)
         (*         let def_entry_pats = List.map (fun _ -> PWild) (tbl_ty.tkey_sizes) in
        let (def_entry:tbl_entry) = {ematch=def_entry_pats; eaction=(Cid.to_id (fst t.tdefault)); eargs=def_install_args;eprio=0;} in *)
         Pipeline.append
           p
           (Pipeline.mk_table
              id
              (extract_int t.tsize)
              (fst t.tdefault |> Cid.to_id, def_acn_args))
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
;;

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


let rec interp_parser_block nst swid payload_id locals parser_block =
  (* interpret the actions, updating locals *)
  let locals = List.fold_left (interp_parser_action nst swid payload_id) locals (List.split parser_block.pactions |> fst) in
  (* now interpret the step *)
  interp_parser_step nst swid payload_id locals (fst parser_block.pstep)
  
and interp_parser_action (nst : State.network_state) swid payload_id locals parser_action = 
  match parser_action with 
  | PRead(cid, ty) -> 
    let payload = get_payload payload_id locals in
    (* semantically, a read creates a new variable and also updates the payload variable *)
    let parsed_val, payload' = InterpPayload.pread (payload) ty in
    (* add the new local, remove old packet, add new packet *)
    locals
    |> Env.add (cid) (InterpSyntax.V(parsed_val))
    |> update_payload payload_id payload'
  | PPeek(cid, ty) -> 
    let peeked_val = InterpPayload.ppeek (get_payload payload_id locals) ty in
    locals |> Env.add (cid) (InterpSyntax.V(peeked_val))
  | PSkip(ty) ->
    let payload' = InterpPayload.padvance (get_payload payload_id locals) ty in
    update_payload payload_id payload' locals
  | PAssign(cid, exp) ->
    let assigned_ival = interp_exp nst swid locals exp in
    locals
    |> Env.remove cid
    |> Env.add (cid) (assigned_ival)
  | PLocal(cid, _, exp) -> 
    let assigned_ival = interp_exp nst swid locals exp in
    Env.add (cid) (assigned_ival) locals

and interp_parser_step nst swid payload_id locals parser_step = 
    match parser_step with
    | PMatch(es, branches) ->
      let vs = List.map (fun e -> interp_exp nst swid locals e |> extract_ival) es in
      let first_match =
        try List.find (fun (pats, _) -> matches_pat vs pats) branches with
        | _ -> error "[interp_parser_step] parser match did not match any branch!"
      in
      interp_parser_block nst swid payload_id locals (snd first_match)
    | PGen(exp) -> (
      let event_val = interp_exp nst swid locals exp |> extract_ival in
      event_val      
    )
    | PCall(exp) -> (
        match exp.e with 
        | ECall(cid, args) -> (
          (* a call to another parser. *)
          (* construct ival arguments *)
          let args = 
            (InterpSyntax.V(port_arg locals))::(List.map (interp_exp nst swid locals) args)
          in
          (* call the parser function as you would any other function *)
          match State.lookup swid cid nst with 
            | F(parser_f) -> parser_f nst swid args
            | _ -> error "[parser call] could not find parser function"
        )
        | _ -> error "[parser call] expected a call expression"

    )
    (* halt processing *)
    | PDrop -> CoreSyntax.vbool false
;;

let rec find_payload_param params = 
  match params with 
  | [] -> None
  | (id, ty)::_ when InterpPayload.is_payload_ty ty -> Some(id)
  | _::tl -> find_payload_param tl
;;

let interp_decl (nst : State.network_state) swid d =
  (* print_endline @@ "Interping decl: " ^ Printing.decl_to_string d; *)
  match d.d with
  | DGlobal (id, ty, e) -> interp_dglobal nst swid id ty e
  | DAction acn ->
    (* add the action to the environment *)
    State.add_action (Cid.id acn.aid) acn nst
  | DHandler (id, hdl_sort, (params, body)) ->(
    (* print_endline@@"Adding handler"^(CorePrinting.id_to_string id);
    print_endline@@"handler sort: "^(match hdl_sort with | HData -> "ingress" | HEgress -> "egress" | _ ->""); *)
    let f nst swid port event =
      if (hdl_sort = HEgress) then 
        print_endline@@"interping egress handler";
      (* add the event to the environment *)
      let builtin_env =
        List.fold_left
          (fun acc (k, v) -> Env.add k v acc)
          Env.empty
          [ Id Builtins.this_id, InterpSyntax.V (vevent { event with edelay = 0 })
          ; Id Builtins.ingr_port_id, InterpSyntax.V (vint port 32) ]
      in
      (* add event parameters to locals *)
      let locals =
        List.fold_left2
          (fun acc v (id, _) -> Env.add (Id id) (InterpSyntax.V v) acc)
          builtin_env
          event.data
          params
      in
      State.update_counter swid event nst; (*TODO: fix counters for egress. *)
      Pipeline.reset_stage nst.switches.(swid).pipeline;
      ignore @@ interp_statement nst hdl_sort swid locals body
    in
    match hdl_sort with
    | HData -> State.add_handler (Cid.id id) f nst
    | HEgress -> State.add_egress_handler (Cid.id id) f nst
    | _ -> error "control handlers not supported"
    )

  (* parsers: convention is for first two arguments to be 
  ingress port and unparsed packet / payload. *)
  | DParser(id, params, parser_block) -> 
    (* figure out whether to use the implicit payload argument *)
    let payload_id_opt = find_payload_param params in
    let runtime_function nst swid args = 
      (* if there is no payload parameter, put one in the front *)
      let param_ids, payload_id = match payload_id_opt with
        | None -> ((Builtins.ingr_port_id)::(Builtins.packet_arg_id)::(List.split params |> fst), Builtins.packet_arg_id)
        | Some(payload_id) -> (Builtins.ingr_port_id)::(List.split params |> fst), payload_id
      in
      (* construct the locals table *)
      let locals = 
        List.fold_left2
          (fun acc v id -> Env.add (Id id) v acc)
          Env.empty
          args
          param_ids
      in
      interp_parser_block nst swid payload_id locals parser_block
    in
    State.add_global swid (Cid.id id) (F runtime_function) nst;
    nst

  | DEvent (id, num_opt, _, _) ->
    (* the expression inside a generate just constructs an event value. *)
    (* the generate statement adds the payload, however *)
    let f _ _ args =
      let event_num_val = match num_opt with
      | None -> None 
      | Some(num) -> Some(vint (size_of_tint (
        SyntaxToCore.translate_ty  
        Builtins.lucid_eventnum_ty)) num)
      in
      (* let extract_ival_pkt_placeholder ival = 
        match ival with
        | State.P(_) -> {v=VPat([]); vty=Payloads.payload_ty |> SyntaxToCore.translate_ty; vspan=Span.default}
        | _ -> extract_ival ival
      in *)
      vevent { 
        eid = Id id; 
        data = List.map extract_ival args; 
        edelay = 0;
        evnum = event_num_val;
    }
    in
    State.add_global swid (Id id) (InterpSyntax.F f) nst;
    nst
  | DMemop { mid; mparams; mbody } ->
    let f = interp_memop mparams mbody in
    State.add_global swid (Cid.id mid) (InterpSyntax.F f) nst;
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

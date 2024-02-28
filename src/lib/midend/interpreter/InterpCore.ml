(* Interpreter evaluation functions *)
open Batteries
open Yojson.Basic
open CoreSyntax
open SyntaxUtils
open InterpState
open InterpSwitch
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
  | Cast (Sz size), [v] -> vinteger (Integer.set_size size (raw_integer v))
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

let get_local id locals = match Env.find (Id id) locals with
  | InterpSyntax.V(v) -> v
  | _ -> error "not a value"
;;
let update_local local_id v locals = 
  Env.add (Id local_id) (InterpSyntax.V(v)) (Env.remove (Id local_id) locals)
;;

let interp_eval exp : 'a InterpSyntax.ival =
  match exp.e with
  | EVal v -> V v
  | _ ->
    error "[interp_eval] expected a value expression, but got something else."
;;

let calc_crc16_csum (zs : zint list) = 
  let full_z = List.fold_left (fun acc z -> Integer.concat acc z) (Integer.create ~value:0 ~size:0) zs in

  (* let bs = List.map (fun z -> BitString.int_to_bits (Z.to_int z.size) (Z.to_int z.value)) zs in *)
  (* let full_b = List.fold_left BitString.concat BitString.empty bs in *)
  (* print_endline ("calculating ipv4 checksum of: "^(BitString.bits_to_hexstr full_b)); *)

  (* print_endline ("size of full_z: "^(string_of_int (Integer.size full_z))); *)
  let full_z = ref full_z in
  let sum = ref (Integer.create ~value:0 ~size:17) in
  (* 16 bits + carry... *)
  while ((Integer.size (!full_z)) > 0)
  do
    (* print_endline "-----";
    print_endline ("full_z: "^(Integer.to_string (!full_z)));
    print_endline ("sum start: "^(Integer.to_string (!sum))); *)
    let msb, rest = Integer.pop_msb (Z.of_int 16) (!full_z) in
    (* print_endline ("msb: "^(Integer.to_string msb));
    print_endline ("rest: "^(Integer.to_string rest)); *)
    full_z := rest;
    (* add and get carry bit *)
    sum := Integer.add (!sum) (Integer.set_size 17 msb);
    (* fold carry bit into the checksum *)
    let carry, sum_rest = Integer.pop_msb (Z.of_int 1) (!sum) in
    let carry = Integer.set_size 16 carry in
    let sum' = Integer.add sum_rest carry in
    (* update the checksum *)
    sum := Integer.set_size 17 sum';
    (* print_endline ("sum end: "^(Integer.to_string (!sum))); *)
  done;
  (* take the ones compliment of the sum *)
  Integer.bitnot (Integer.set_size 16 !sum)
;;

let rec interp_exp (nst : State.network_state) swid locals e : 'a InterpSyntax.ival =
  let interp_exps = interp_exps nst swid locals in
  let interp_exp = interp_exp nst swid locals in
  let extract_int = function
    | VInt n -> n
    | _ -> failwith "No good"
  in
  let lookup cid = lookup_var swid nst locals cid in
  match e.e with
  | EVal v -> V v
  | EVar cid -> 
    let res = lookup cid in
    res
  | EOp (op, es) ->
    let vs = interp_exps es in
    V (interp_op op vs)
  | ECall (cid, es, _) ->
    let vs = interp_exps es in
    (match lookup cid with
     | V _ ->
       error
         (Cid.to_string cid
         ^ " is a value identifier and cannot be used in a call")
     | F f -> f nst swid vs
   )
  | EHash (Szs _, _ ) -> 
    error "Hash expression size should not be a tuple"
  | EHash (Sz size, args) -> (
    let poly, args = List.hd args, List.tl args in
    let vs = List.map (fun ival -> (extract_ival ival).v) (interp_exps args) in
    match poly.e with
    (* special case 1: do a checksum calculation *)
    | EVar(cid) when (Cid.equal (cid) (Cid.id Builtins.checksum_id)) -> (
      let zs = List.map extract_int vs in
      let csum = calc_crc16_csum zs in
      V (VInt csum |> value)
    )
    | _ -> (
      let seed = (interp_exp poly |> extract_ival).v in
      let vs = seed::vs in
      (match vs with
      | VInt seed :: tl ->
        (* Special case 2: if the hash seed is 1 and all the arguments are integers,
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
    )
  )
  | EFlood e1 ->
    let port =
      interp_exp e1
      |> extract_ival
      |> raw_integer
      |> Integer.to_int
    in
    V (vgroup [-(port + 1)])
  | ERecord(fields) -> 
    (* interp each field to produce a VRecord((id * v) list) *)
    let fields = List.map 
      (fun (id, e) -> 
        let v = (interp_exp e |> extract_ival).v in
        (id, v)) 
      fields 
    in
    V (vrecord fields)
  | EProj(e, id) ->
    let v = interp_exp e |> extract_ival in
    (match v.v with
     | VRecord(fields) -> (
       try
         let v = List.assoc id fields in
         V (value v)
       with
       | Not_found -> error ("Field " ^ Id.to_string id ^ " not found"))
      | _ -> error "Not a record")
  | ETuple(es) -> 
    let ivals = interp_exps es in
    (* HACK: Table.create takes a list of action constructors as an argument. 
       Action constructors are function IVals, which can't be converted into a 
       raw value. So we just drop those values. *)
    let vs = try 
      List.map (fun ival -> (extract_ival ival).v) ivals
    with
      Failure _ -> []
    in
      V (vtup vs Span.default)

    (* V (VRecord(fields)) *)

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
        | PEvent (eid, _), VEvent ev ->
          (String.equal (CorePrinting.cid_to_string eid) (CorePrinting.cid_to_string ev.eid)) 
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

let printf_string swid str = 
  if Cmdline.cfg.json || Cmdline.cfg.interactive
    then InterpJson.interp_report_json "printf" str (Some swid)
    else str    

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

(* convert a flood port into a list of declared ports *)
let expand_flood_port nst swid flood_port =
  List.filter_map
    (fun (port, dst_swid) -> 
      if (port <> (-(flood_port + 1))) && (dst_swid <> swid) 
        then Some(port)
        else None)
    (State.ports_to_neighbors nst.State.links swid)
;;

let rec interp_statement nst hdl_sort swid locals s =
  (* (match s.s with
  | SSeq _ | SNoop -> () (* We'll print the sub-parts when we get to them *)
  | _ -> print_endline @@ "Interpreting " ^ CorePrinting.stmt_to_string s); *)
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
    if (Cmdline.cfg.show_printf) then (
      let vs = List.map (fun e -> interp_exp e |> extract_ival) es in
      let strout = printf_replace vs s in
      printf_string swid strout |> print_endline);
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
    let event = interp_exp e |> extract_ival |> raw_event in

    match hdl_sort with 
    | HData -> (
      (*  
        event routing is overly complicated.
        cases:
        generate_switch -- send directly to switch ingress
        generate_port -- send to local egress port
        generate -- send to local egress recirculation port
        generate_ports -- send to multiple local egress ports

        ports are either: 
        1. connected to another switch
        2. not connected to another switch, in which case an exit event happens

        todo: 
        1. an explicit exit port for each switch
        2. generate_switch should be depreciated
      *)
      let output_ports = match g with
        (* recirculation *)
        | GSingle None ->
          [ Port(State.lookup swid (Cid.from_string "recirculation_port") nst
          |> extract_ival
          |> raw_integer
          |> Integer.to_int )  ]
        (* teleport to switch ingress. should be depreciated *)
        | GSingle (Some exp) -> 
          let swid = interp_exp exp |> extract_ival |> raw_integer |> Integer.to_int in
          [(Switch(swid))]
        (* generate to a port in the switch *)
        | GPort port -> [Port(interp_exp port |> extract_ival |> raw_integer |> Integer.to_int)]
        (* generate to multiple ports *)
        | GMulti grp -> 
          let ports = interp_exp grp |> extract_ival |> raw_group in
          (match ports with
          | [port] when port < 0 -> (* flood to all ports except: -(port + 1) *)
            (* if we're flooding, we should also generate an event to the "exit" node in the network.  *)
              PExit(port)::(List.map (fun p-> Port(p)) (expand_flood_port nst swid port))              
          | _ -> 
            List.map (fun port -> Port(port)) ports)
      in
      (* push all the events to output ports *)
      List.iter (fun out_port -> 
        InterpSwitch.ingress_send nst (State.lookup_switch nst swid) out_port event) 
        output_ports;
      locals       
    )
    | HEgress -> (
      let extract_int = function
        | VInt n -> n
        | _ -> failwith "No good"
      in
      (* egress case -- always just push the event to the other side of the port *)
      (* note that we ignore any location arguments and treat all 
         egress generate variants the same! *)
      let port = ((port_arg locals).v |> extract_int ).value |> Z.to_int in 
      (* egress serializes packet events *)
      let ev_sort = Env.find event.eid nst.event_sorts in
      (* serialize packet events *)
      let event_val = match ev_sort with 
        | EBackground -> 
          event (* background events stay as events *)
        | EPacket -> 
          InterpPayload.serialize_packet_event event
      in
      InterpSwitch.egress_send nst (State.lookup_switch nst swid) port event_val;
      locals
    )
    | HControl -> (error "control events are not implemented")
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
    let update_local locals p v = 
      match p,v.v with
      | PEvent (_, params), VEvent (ev) -> (List.fold_left2
          (fun acc v (id, _) -> Env.add (Id id) ((V v): 'a InterpSyntax.ival) acc)
          locals
          ev.data
          params)
      | _,_ -> locals 
    in
    (* expand a single wildcard to multiple wildcards for the fold *)
    let first_match = match first_match with 
      | ([PWild], ss) -> List.init (List.length es) (fun _ -> PWild), ss
      | _ -> first_match
    in
    let locals = List.fold_left2 update_local locals (fst first_match) vs in
    interp_statement nst hdl_sort swid locals (snd first_match)
  | STupleAssign({ids; exp}) ->
    (* eval the exp, get a list of results, assign them to the ids in locals *)
    let v_result = interp_exp exp |> extract_ival in
    let vs = match v_result.v with
      | VTuple(vs) -> vs
      | _ -> error "not a tuple"
    in
    List.fold_left2 
      (fun locals v id -> Env.add (Id id) (InterpSyntax.V v) locals) 
      locals 
      (List.map (fun v -> value v) vs)
      ids
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
    | TName (cid, sizes) -> Cid.names cid, sizes
    | _ -> failwith "Bad DGlobal"
  in
  let args =
    match e.e with
    | ECall (_, args, _) -> args
    | _ -> failwith "Bad constructor"
  in
  let new_p =
    match gty_name, gty_sizes, args with
    | ["Array"; "t"], [Sz size], [e] ->
      let len =
        interp_exp nst swid Env.empty e
        |> extract_ival
        |> raw_integer
        |> Integer.to_int
      in
      Pipeline.append p (Pipeline.mk_array id size len false)
    | ["Counter"; "t"], [Sz size], [e] ->
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
    | ["PairArray"; "t"], [Sz size], [e] ->
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
  State.add_global swid (Id id) (V (vglobal id idx ty)) nst;
  nst
;;

let interp_dglobal (nst : State.network_state) swid id ty e =
  match e.e with 
  | ECall(cid, args, _) when (Cid.names cid) = ["Table"; "create"] -> (
    (* eval the args *)
    let arg_ivals = List.map (fun e -> interp_exp nst swid Env.empty e) args in
    (* construct the value *)
    let idx = Pipeline.length (nst.switches.(swid).pipeline) in
    let vg_ival = InterpSyntax.V (vglobal id idx ty) in
    (* call the constructor to update the pipeline, adding the value to it *)
    let arg_ivals = vg_ival::arg_ivals in
    let new_pipe = Tables.create_ctor nst swid arg_ivals in 
    (* update the global state's pipeline *)
    nst.switches.(swid) <- { nst.switches.(swid) with pipeline = new_pipe };
    (* add the global to globals context in nst *)
    State.add_global swid (Id id) vg_ival nst;
    (* return updated nst *)
    nst
    (* interp_dtable nst swid id ty e *)
  )
  (* old builtin method of constructing globals. 
     TODO: put the constructors into a global context and refactor 
           to use the same approach as for tables, above. 
           Eventually, a constructor call should be implemented the same way as a 
           function call (it just happens to be one that updates the network state) *)
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
  | MBComplex body -> InterpSyntax.V(interp_complex_body params body nst swid args)
  | MBReturn e ->
    let locals =
      List.fold_left2
        (fun acc arg (id, _) -> Env.add (Id id) arg acc)
        Env.empty
        args
        params
    in
    interp_exp nst swid locals e
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
    then interp_exp nst swid locals e2
    else interp_exp nst swid locals e3
;;



let rec interp_parser_block nst swid payload_id locals parser_block =
  (* interpret the actions, updating locals *)
  let locals = List.fold_left (interp_parser_action nst swid payload_id) locals (List.split parser_block.pactions |> fst) in
  (* now interpret the step *)
  interp_parser_step nst swid payload_id locals (fst parser_block.pstep)
  
and interp_parser_action (nst : State.network_state) swid payload_id locals parser_action = 
  (* TODO: implement Payload.read and Payload.peek *)
  match parser_action with 
  | PRead(cid, ty, _) -> 
    let payload = get_local payload_id locals in
    (* semantically, a read creates a new variable and also updates the payload variable *)
    let parsed_val, payload' = InterpPayload.pread payload ty in
    (* add the new local and update payload variable *)
    locals
      |> Env.add (cid) (InterpSyntax.V(parsed_val))
      |> update_local payload_id payload'
  | PPeek(cid, ty, _) -> 
    let peeked_val = InterpPayload.ppeek (get_local payload_id locals) ty in
    locals |> Env.add (cid) (InterpSyntax.V(peeked_val))
  | PSkip(ty) ->
    let payload' = InterpPayload.padvance (get_local payload_id locals) ty in
    update_local payload_id payload' locals
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
        | ECall(cid, args, _) -> (
          (* a call to another parser. *)
          (* construct ival arguments *)
          let args = 
            (InterpSyntax.V(port_arg locals))::(List.map (interp_exp nst swid locals) args)
          in
          (* call the parser function as you would any other function *)
          match State.lookup swid cid nst with 
            | F(parser_f) -> let rv = parser_f nst swid args in rv |> extract_ival
            | _ -> error "[parser call] could not find parser function"
        )
        | _ -> error "[parser call] expected a call expression"

    )
    (* halt processing *)
    | PDrop -> CoreSyntax.vbool false
;;

let rec find_bitstring_param params = 
  match params with 
  | [] -> None
  | (id, ty)::_ when (ty.raw_ty = TBits(Sz 1500)) -> Some(id)
  | _::tl -> find_bitstring_param tl
;;

let interp_decl (nst : State.network_state) swid d =
  (* print_endline @@ "Interping decl: " ^ Printing.decl_to_string d; *)
  match d.d with
  | DGlobal (id, ty, e) -> interp_dglobal nst swid id ty e
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
      State.update_counter swid event nst; (*TODO: why are we counting packet events here? *)
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
    (* figure out whether to use the implicit payload argument. if there is an explicit 
        payload for the parser, it must be the first argument. *)
    let payload_id_opt = find_bitstring_param params in
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
      InterpSyntax.V(interp_parser_block nst swid payload_id locals parser_block)
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
      InterpSyntax.V (vevent { 
        eid = Id id; 
        data = List.map extract_ival args; 
        edelay = 0;
        evnum = event_num_val;
        eserialized = false;
    })
    in
    State.add_global swid (Id id) (InterpSyntax.F f) nst;
    nst
  | DMemop { mid; mparams; mbody } ->
    let f = interp_memop mparams mbody in
    State.add_global swid (Cid.id mid) (InterpSyntax.F f) nst;
    nst
  | DExtern _ ->
    failwith "Extern declarations should be handled during preprocessing"
  | DUserTy _ -> nst (*all user types should be inlined by now*)
  | DFun(id, _, body) -> 
    let runtime_function (nst: State.network_state) swid args = 
      (* bind args to parameters *)
      let locals = 
        List.fold_left2
          (fun acc v id -> Env.add (Id id) v acc)
          Env.empty
          args 
          (fst body |> List.split |> fst)
      in
      (* no need to reset the pipe stage -- main should start at the beginning. *)
      (* Pipeline.reset_stage nst.switches.(swid).pipeline; *)
      (* interp the statement *)
      let _ = interp_statement  nst HData swid locals (snd body) in
      let ret_v = match (!(nst.switches.(swid).retval)) with 
        | Some(v) -> v
        | None -> vint 0 0;
      in
      nst.switches.(swid).retval := None;
      InterpSyntax.V(ret_v)
    in 
    State.add_global swid (Cid.id id) (F runtime_function) nst;
    nst

  | DActionConstr({aid; aconst_params; aparams; abody}) -> 
    (* add a function to the environment that takes the action constructor's params 
       and returns a function version of the inner action *)
    let action_function_generator _ _ const_args = 
      print_endline ("in action_function_generator with args: "^(List.map (fun v -> (extract_ival v) |> Printing.value_to_string) const_args |> String.concat ", "));
      (* the inner action function *)
      let action_function _ _ args = 
        print_endline ("in action_function with args: "^(List.map (fun v -> (extract_ival v) |> Printing.value_to_string) args |> String.concat ", "));
        (* bind the closure args and runtime args in the env *)
        let locals = 
          List.fold_left2
            (fun acc v id -> Env.add (Id id) v acc)
            Env.empty
            (const_args@args)
            ((aconst_params|> List.split |> fst)@(aparams |> List.split |> fst))
        in
        let ret_vs = List.map 
          (fun exp -> (interp_exp nst swid locals exp |> extract_ival).v)
          abody 
        in
        let ret_v = value@@VTuple(ret_vs) in
        InterpSyntax.V(ret_v)
      in
      let action_f = InterpSyntax.F action_function in
      action_f
    in
    let constr_f = InterpSyntax.F action_function_generator in
    State.add_global swid (Cid.id aid) constr_f nst;    
    nst   
;;


let process_decls nst ds =
  let rec aux i (nst : State.network_state) =
    if i = Array.length nst.switches
    then nst
    else aux (i + 1) (List.fold_left (fun nst -> interp_decl nst i) nst ds)
  in
  aux 0 nst
;;

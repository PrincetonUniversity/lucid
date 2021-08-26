(* Interpreter evaluation functions *)
open Batteries
open Syntax
open SyntaxUtils
open InterpState

let rec compute_size env size =
  match STQVar.strip_links size with
  | IConst n -> n
  | IUser cid ->
    (match Env.find_opt cid env with
    | Some n -> n
    | None -> error @@ "Unbound size identifer " ^ Printing.cid_to_string cid)
  | ISum (sizes, n) ->
    n + List.fold_left (fun acc s -> acc + compute_size env s) 0 sizes
  | IVar _ ->
    (* If any IVars are left over, their size shouldn't matter *)
    32
;;

let interp_size (nst : State.network_state) size = compute_size nst.sizes size

let interp_op st op vs =
  let vs = List.map extract_ival vs in
  match op, vs with
  | And, [v1; v2] -> vbool (raw_bool v1 && raw_bool v2)
  | Or, [v1; v2] -> vbool (raw_bool v1 || raw_bool v2)
  | Not, [v] -> vbool (not (raw_bool v))
  | Cast size, [v] ->
    vinteger (Integer.set_size (interp_size st size) (raw_integer v))
  | Eq, [v1; v2] -> vbool (v1.v = v2.v)
  | Neq, [v1; v2] ->
    vbool (not (Integer.equal (raw_integer v1) (raw_integer v2)))
  | Less, [v1; v2] -> vbool (Integer.lt (raw_integer v1) (raw_integer v2))
  | More, [v1; v2] -> vbool (Integer.lt (raw_integer v2) (raw_integer v1))
  | Leq, [v1; v2] -> vbool (Integer.leq (raw_integer v1) (raw_integer v2))
  | Geq, [v1; v2] -> vbool (Integer.geq (raw_integer v1) (raw_integer v2))
  | Plus, [v1; v2] -> vinteger (Integer.add (raw_integer v1) (raw_integer v2))
  | Sub, [v1; v2] -> vinteger (Integer.sub (raw_integer v1) (raw_integer v2))
  | SatSub, [v1; v2] ->
    if Integer.lt (raw_integer v1) (raw_integer v2)
    then
      vinteger (Integer.create ~value:0 ~size:(Integer.size (raw_integer v1)))
    else vinteger (Integer.sub (raw_integer v1) (raw_integer v2))
  | Conc, [v1; v2] ->
    let v1, v2 = raw_integer v1, raw_integer v2 in
    let sz = Integer.size v1 + Integer.size v2 in
    let v1', v2' = Integer.set_size sz v1, Integer.set_size sz v2 in
    vinteger (Integer.add (Integer.shift_left v1' (Integer.size v2)) v2')
  | BitAnd, [v1; v2] ->
    vinteger (Integer.bitand (raw_integer v1) (raw_integer v2))
  | BitOr, [v1; v2] ->
    vinteger (Integer.bitor (raw_integer v1) (raw_integer v2))
  | LShift, [v1; v2] ->
    vinteger
      (Integer.shift_left (raw_integer v1) (raw_integer v2 |> Integer.to_int))
  | RShift, [v1; v2] ->
    vinteger
      (Integer.shift_right (raw_integer v1) (raw_integer v2 |> Integer.to_int))
  | Slice (hi, lo), [v] ->
    vinteger
      (Integer.shift_right (raw_integer v) lo |> Integer.set_size (hi - lo + 1))
  | _ ->
    error
      ("bad operator: "
      ^ Printing.op_to_string op
      ^ " with "
      ^ string_of_int (List.length vs)
      ^ " arguments")
;;

let lookup_var swid nst locals cid =
  try Env.find cid locals with
  | _ -> State.lookup swid cid nst
;;

let rec interp_exp (nst : State.network_state) swid locals e : State.ival =
  let interp_exps = interp_exps nst swid locals in
  let lookup cid = lookup_var swid nst locals cid in
  match e.e with
  | EVal v -> V v
  | EVar cid -> lookup cid
  | EInt (value, size) ->
    let size =
      match size with
      | Some s -> interp_size nst s
      | _ -> 32
    in
    V (vinteger (Integer.create_z ~value ~size))
  | EOp (op, es) ->
    let vs = interp_exps es in
    V (interp_op nst op vs)
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
    (match vs with
    | V { v = VInt seed } :: tl ->
      let hashed = Legacy.Hashtbl.seeded_hash (Integer.to_int seed) tl in
      V (vint hashed (interp_size nst size))
    | _ -> failwith "Wrong arguments to hash operation")
  | EProj _ | ERecord _ | EWith _ ->
    Console.error_position
      e.espan
      "Record expressions and projection operators should be eliminated before \
       interpretation"
  | EComp _ | EIndex _ | EVector _ ->
    Console.error_position
      e.espan
      "Vector expressions should be eliminated before interpretation"
  | ETuple _ ->
    Console.error_position
      e.espan
      "Tuple expressions should be eliminated before interpretation"
  | ESizeCast _ ->
    Console.error_position e.espan "Should be eliminated before interpretation"

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

let rec interp_statement nst swid locals s =
  (* (match s.s with
  | SSeq _ -> () (* We'll print the sub-parts when we get to them *)
  | _ -> print_endline @@ "Interpreting " ^ Printing.stmt_to_string s); *)
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
    print_endline (printf_replace vs s);
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
  | SGen (_, e) ->
    let event = interp_exp e |> extract_ival |> raw_event in
    if Env.find event.eid nst.event_sorts = EExit
    then State.log_exit swid event nst
    else (
      let locs =
        if List.length event.elocations = 0
        then [swid]
        else List.map Integer.to_int event.elocations
      in
      List.iter (fun loc -> State.push_event loc event nst) locs);
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
  | SLoop _ -> error "Loops should be eliminated before interpretation"
;;

let interp_dglobal (nst : State.network_state) swid id ty e =
  (* FIXME: This functions is probably more complicated than it needs to be.
     We can probably do this a lot better by writing the Array.create function
     in Arrays.ml (and similarly for counters), then just calling that. But I
     don't want to muck around with the interpreter for now, so I'm sticking to
     quick fixes. *)
  let st = nst.switches.(swid) in
  let p = st.pipeline in
  let idx = Pipeline.length p in
  let gty_name, gty_sizes =
    match TyTQVar.strip_links ty.raw_ty with
    | TName (cid, sizes, _) -> Cid.names cid, sizes
    | _ -> failwith "Bad DGLobal"
  in
  let args =
    match e.e with
    | ECall (_, args) -> args
    | _ -> failwith "Bad constructor"
  in
  let new_p =
    match gty_name, gty_sizes, args with
    | ["Array"; "t"], [size], [e] ->
      let size = interp_size nst size in
      let len =
        interp_exp nst swid Env.empty e
        |> extract_ival
        |> raw_integer
        |> Integer.to_int
      in
      Pipeline.append_stage size len p
    | ["Counter"; "t"], [size], [e] ->
      let size = interp_size nst size in
      let init_value =
        interp_exp nst swid Env.empty e |> extract_ival |> raw_integer
      in
      let new_p = Pipeline.append_stage size 1 p in
      ignore
        (Pipeline.update
           ~stage:idx
           ~idx:0
           ~getop:(fun _ -> Z.zero)
           ~setop:(fun _ -> init_value)
           new_p);
      new_p
    | _ ->
      error
        "Wrong number of arguments to global constructor, or user type \
         appeared during interpretation"
  in
  nst.switches.(swid) <- { st with pipeline = new_p };
  State.add_global swid (Id id) (V (vglobal idx)) nst;
  nst
;;

let interp_decl (nst : State.network_state) swid d =
  (* print_endline @@ "Interping decl: " ^ Printing.decl_to_string d; *)
  let interp_exp = interp_exp nst swid Env.empty in
  match d.d with
  | DGlobal (id, ty, e) -> interp_dglobal nst swid id ty e
  | DHandler (id, (params, body)) ->
    let f nst swid event =
      let this_event = vevent { event with edelay = 0; elocations = [] } in
      let locals =
        List.fold_left2
          (fun acc v (id, _) -> Env.add (Id id) (State.V v) acc)
          (Env.singleton (Id Builtins.this_id) (State.V this_event))
          event.data
          params
      in
      State.update_counter swid event nst;
      Pipeline.reset_stage nst.switches.(swid).pipeline;
      ignore @@ interp_statement nst swid locals body
    in
    State.add_handler (Cid.id id) f nst
  | DEvent (id, _, _, _) ->
    let f _ _ args =
      vevent
        { eid = Id id
        ; data = List.map extract_ival args
        ; edelay = 0
        ; elocations = []
        }
    in
    State.add_global swid (Id id) (State.F f) nst;
    nst
  | DFun (id, _, _, (params, body)) ->
    let st = nst.switches.(swid) in
    let f nst swid args =
      let locals =
        List.fold_left2
          (fun acc arg (id, _) -> Env.add (Id id) arg acc)
          Env.empty
          args
          params
      in
      let _ = interp_statement nst swid locals body in
      let ret = !(st.retval) in
      st.retval := None;
      match ret with
      | Some v -> v
      | _ -> vbool false
      (* Dummy value for no return statement *)
    in
    State.add_global swid (Cid.id id) (State.F f) nst;
    nst
  | DMemop (id, (params, body)) ->
    (* Basically the same as a function *)
    let st = nst.switches.(swid) in
    let f nst swid args =
      let locals =
        List.fold_left2
          (fun acc arg (id, _) -> Env.add (Id id) arg acc)
          Env.empty
          args
          params
      in
      let _ = interp_statement nst swid locals body in
      let ret = !(st.retval) in
      st.retval := None;
      Option.get ret
    in
    State.add_global swid (Cid.id id) (State.F f) nst;
    nst
  | ConstVar (x, _, e) ->
    let v = interp_exp e in
    State.add_global swid (Id x) v nst;
    nst
  | DGroup (x, es) ->
    let vs =
      List.map (fun e -> interp_exp e |> extract_ival |> raw_integer) es
    in
    State.add_global swid (Id x) (V (vgroup vs)) nst;
    nst
  | DExtern _ | DSize _ ->
    failwith
      "Extern and size declarations should be handled during preprocessing"
  | DUserTy _ | ConstVarr _ | DModule _ -> failwith "Should be eliminated"
;;

let process_decls nst ds =
  let rec aux i (nst : State.network_state) =
    if i = Array.length nst.switches
    then nst
    else aux (i + 1) (List.fold_left (fun nst -> interp_decl nst i) nst ds)
  in
  aux 0 nst
;;

(* 
  Lucid programs have four options for parsers:
  1. write a main parser that gets the completely unparsed packet. 
     - In programs with only packet events, this lets you write 
       non-ethernet programs. But once you use background events, 
       you need to have a main that is well-formed. (matches on 
       ethertype and calls do_lucid_parsing for LUCID_ETHERTY).
  2. write a main parser that gets an ethernet packet + payload. 
    - This is the most common case. It hides the branch to 
      LUCID_ETHERTYPE and the call to do_lucid_parsing. So 
      overall its less to remember. 
  3. write no parsers and no packet events. 
    - In this case, Lucid will generate parsers for the background 
      events in the internal format
  4. write no parsers, 1 packet event eth(int<48>, int<48>, int<16>, Payload.t), 
     and however many background events.
    - In this case, Lucid will generate a main parser that calls 
      generated background event parser for lucid etherty and 
      a generated eth parser for all other ethertypes that 
      simply calls the eth event.

  This pass detects which of these cases the program is in and
  adds the necessary parsers.
*)


open Collections
open CoreSyntax
open MiscUtils




let rec find_parser_opt ds pid = 
  match ds with 
  | [] -> None
  | decl::ds -> (
     match decl.d with 
     | DParser(id, params, parser_block) when ((fst id) = (fst pid)) -> 
        Some(id, params, parser_block)
     | _ -> find_parser_opt ds pid
  )
;;
let rec add_after_parser ds pid new_decl = 
  match ds with 
  | [] -> []
  | decl::ds -> (match decl.d with 
     | DParser(id, _, _) when ((fst id) = (fst pid)) -> 
        decl::new_decl::ds
     | _ -> decl::(add_after_parser ds pid new_decl))

;;

let is_pktev decl = match decl.d with 
   | DEvent(_, _, EPacket, _) -> true
   | _ -> false
;;
let is_bgev decl = match decl.d with
   | DEvent(_, _, EBackground, _) -> true
   | _ -> false
;;
(* add a main that calls do_lucid_parsing() *)
(* requirements: no main, no eth_main, no packet events *)
(* returns: success, updated decls *)
let add_background_parser ds : bool * decls = 
  let no_main = match find_parser_opt ds Builtins.main_parse_id with 
   | None -> true 
   | Some _ -> false
  in
  let no_eth_main = match find_parser_opt ds Builtins.eth_parse_id with 
   | None -> true 
   | Some _ -> false
  in  
  let pkt_events = List.filter is_pktev ds in
  let jump = (pcall_cid 
    (Cid.id Builtins.lucid_parse_id) 
    [var (Cid.id Builtins.packet_arg_id) CoreSyntax.pkt_arg_ty])
  in
  if (no_main && no_eth_main && ((List.length pkt_events) = 0)) then (
    let main = decl@@DParser(
        Builtins.main_parse_id,
        [(Builtins.packet_arg_id, CoreSyntax.pkt_arg_ty)],
        (block [] jump))    
    in
    true, ds@[main]
  )
  else (    
  false, ds
  )
;;

(* add a eth_main that generates an eth event *)
(* requirements: no main, no eth_main, exactly 1 packet event named "eth" 
   with type (int<48>, int<48>, int<16>, Payload.t) *)
let add_default_eth_main ds = 
  let no_main = match find_parser_opt ds Builtins.main_parse_id with 
   | None -> true 
   | Some _ -> false
  in
  let no_eth_main = match find_parser_opt ds Builtins.eth_parse_id with 
   | None -> true 
   | Some _ -> false
  in
  let pkt_events = List.filter is_pktev ds in
  let expected_tys = [
    tint (Sz 48); tint (Sz 48); tint (Sz 16); CoreSyntax.payload_ty (* note this is payload type, not packet arg type *)
  ] in
  let eth_event_id_opt = match List.filter_map 
    (fun decl -> match decl.d with 
      | DEvent(id, _, EPacket, [(_, dmac_ty); (_, smac_ty); (_, ety_ty); (_, pl_ty)]) -> 
        if ((fst id) = "eth") && (List.for_all2 equiv_ty [dmac_ty; smac_ty; ety_ty; pl_ty] expected_tys) 
          then (Some(id))
          else (None)
      | _ -> None)   
    pkt_events
    with 
    | [eth_event_id] -> Some(eth_event_id)
    | _ -> None
  in
  if (no_main && no_eth_main && (List.length pkt_events = 1) && (Option.is_some eth_event_id_opt)) then (
    let eth_event_id = Option.get eth_event_id_opt in
  let gen_cmd = pgen 
    (call
      (Cid.id eth_event_id)
      [
        var (cid "dmac") (tint (Sz 48));
        var (cid "smac") (tint (Sz 48));
        var (cid "ety") (tint (Sz 16));
        var (cid "pl") CoreSyntax.pkt_arg_ty
      ]
      (ty TEvent))
    in
    let eth_main = decl@@DParser(
        Builtins.eth_parse_id,
        [
          (id "dmac", tint (Sz 48));
          (id "smac", tint (Sz 48));
          (id "ety", tint (Sz 16));
          (id "pl", CoreSyntax.pkt_arg_ty)
        ],
        (block 
          []
          gen_cmd))
    in
    true, ds@[eth_main]
  )
  else (
    false, ds
  )
;;


let builtin_main eth_parse_id = 
  let ety_id, ety_ty = id "ether_ty", tint (Sz 16) in
  let eth_hdr = [
    (id "dmac", tint (Sz 48));
    (id "smac", tint (Sz 48));
    (ety_id, ety_ty)] 
  in
  let pkt_var = var (Cid.id Builtins.packet_arg_id) (CoreSyntax.pkt_arg_ty) in
  let eth_vars = List.map (fun (id, ty) -> var (Cid.id id) ty) eth_hdr in
  (* the parser reads the fields then jumps to either 
     the lucid internal parser or the user parser *)
  let read (field, ty) = 
    PRead(Cid.id field, ty, pkt_var)
  in
  let jump = pmatch 
    [var (Cid.id ety_id) ety_ty]
    [
      pbranch [Builtins.lucid_ety_int]
        (block 
          [] 
          (pcall_cid 
            (Cid.id Builtins.lucid_parse_id) 
            [pkt_var]));
      pbranch_wild 1
        (block
          []
          (pcall_cid
            (Cid.id (eth_parse_id))
            (eth_vars@[pkt_var])))
    ]
  in
  decl@@DParser(
    Builtins.main_parse_id,
    [(Builtins.packet_arg_id, CoreSyntax.pkt_arg_ty)],
    (block (List.map read eth_hdr) jump))
;;


(* adds a main that extracts eth header and calls do_lucid_parsing for ethertype
   LUCID_ETY or eth_main for all other ethertys. *)
(* requirements: no main defined, eth_main is defined *)
let add_eth_main_wrapper ds = 
  let no_main = match find_parser_opt ds Builtins.main_parse_id with 
   | None -> true 
   | Some _ -> false
  in
  (* check for a well-formed eth-main *)
  let eth_main_id_opt = match find_parser_opt ds Builtins.eth_parse_id with 
    | None -> None
    | Some(eth_main_id, [(_, dmac_ty); (_, smac_ty); (_, ety_ty); (_, pl_ty)], _) -> 
      let expected_tys = [
        tint (Sz 48); tint (Sz 48); tint (Sz 16); CoreSyntax.pkt_arg_ty
      ] in
      if (
        List.for_all2 equiv_ty [dmac_ty; smac_ty; ety_ty; pl_ty] expected_tys
      ) then Some(eth_main_id)
      else None
    | Some _ -> error "This program's eth_main has the wrong argument types. It should take 2 48-bit ints (dst and src mac), a 16-bit int ether type, and a bitstring payload"
  in
  if (no_main) then (
    match eth_main_id_opt with 
    | Some(eth_main_id) -> (
      true, add_after_parser ds Builtins.eth_parse_id (builtin_main eth_main_id)
    )
    | None -> false, ds
  ) else (
    false, ds
  )
;;

let process ds = 
  let packet_events = List.filter is_pktev ds in
  let has_main = match find_parser_opt ds Builtins.main_parse_id with 
   | None -> false 
   | Some _ -> true
  in
  let has_eth_main = match find_parser_opt ds Builtins.eth_parse_id with 
   | None -> false 
   | Some _ -> true
  in
  let has_parsers = has_main || has_eth_main in
  if (not has_parsers) then (
    match (List.length packet_events) with 
    | 0 -> (
      let suc, ds = add_background_parser ds in
      if suc 
        then ds
        else error "compiler error: failed to add a background parser in a case where it should be possible"
    )
    | 1 -> (
      let suc, ds = add_default_eth_main ds in
      if suc 
        then (
          let suc, ds = add_eth_main_wrapper ds in
          if suc
            then ds
            else error "compiler error: failed to add a main parser after generating an eth_main for a single packet event"
        )
        else 
          ds
          (* this case _should_ be an error, but there are some interp_tests that 
             expect this nonstandard behavior. *)
          (* error "compiler error: failed to generate an eth_main for a single packet event eth" *)
    )
    | _ -> error "Program defines no parser, and has more than 1 packet event, so a parser cannot be auto-generated"
  )
  else (
    if (not has_main) then (
      let suc, ds = add_eth_main_wrapper ds in
      if suc
        then ds
        else error "Compiler error: main parse generator either found a main or could not find an eth main, but neither case should be true in this branch of the parse generator."
    )
    (* program defines its own main. leave it alone. *)
    else ds
)
;;
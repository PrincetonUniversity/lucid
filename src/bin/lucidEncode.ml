(* lucidEncode: encode an interpreter spec's events as wire bytes.
     lucidEncode prog.dpt --spec case.json -o OUTDIR

    output:
     OUTDIR/in_<port>.pcap -- per-port event pcaps with timestamps
     OUTDIR/expected.json  -- expected output events on all ports with
                              serialized form in "wire_bytes" field

   This tool works by running a program through the frontend 
   (for event signatures) and serializing events with the 
   interpreter's deparser. *)
open Batteries
open Dpt
open InterpJson

let spec_file = ref ""
let out_dir = ref ""

let speclist =
  [ "--spec", Arg.Set_string spec_file, "Path to the interpreter specification file"
  ; "-o", Arg.Set_string out_dir, "Output directory" ]
;;

let fail msg =
  prerr_endline ("lucidEncode: " ^ msg);
  exit 1
;;

(*** pcap writing ***)
let le32 buf n =
  for i = 0 to 3 do
    Buffer.add_char buf (Char.chr ((n lsr (8 * i)) land 0xff))
  done
;;

let le16 buf n =
  Buffer.add_char buf (Char.chr (n land 0xff));
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff))
;;

(* a nanosecond-precision, ethernet-linktype pcap of (time_ns, frame) records *)
let write_pcap path records =
  let buf = Buffer.create 4096 in
  le32 buf 0xa1b23c4d;
  le16 buf 2;
  le16 buf 4;
  le32 buf 0;
  le32 buf 0;
  le32 buf 65535;
  le32 buf 1;
  List.iter
    (fun (t, frame) ->
      le32 buf (t / 1_000_000_000);
      le32 buf (t mod 1_000_000_000);
      le32 buf (String.length frame);
      le32 buf (String.length frame);
      Buffer.add_string buf frame)
    records;
  let oc = Stdlib.open_out_bin path in
  Stdlib.output_string oc (Buffer.contents buf);
  Stdlib.close_out oc
;;

let hex s = BitString.of_byte_string s |> BitString.bits_to_hexstr

(*** spec checks ***)
let check_single_node (topo : InterpTopo.topology option) =
  match topo with
  | Some topo ->
    if List.length topo.nodes <> 1 then fail "spec must have exactly one node";
    if topo.links <> [] then fail "spec must not have links"
  | None -> fail "spec must have a topology block (with a single node)"
;;

(* the input's port; an input must be located at switch 0 and one port *)
let input_port ilocs =
  match ilocs with
  | [ { InterpSyntax.switch = Some 0; port } ] -> port
  | _ -> fail "each input must be located at exactly one port of switch 0 (\"0:<port>\")"
;;

(*** encoding ***)
let main () =
  Arg.parse speclist (fun s -> Config.base_cfg.dpt_file <- s)
    "lucidEncode prog.dpt --spec case.json -o OUTDIR. Options:";
  let dpt_file = Config.base_cfg.dpt_file in
  if dpt_file = "" || !spec_file = "" || !out_dir = ""
  then fail "usage: lucidEncode prog.dpt --spec case.json -o OUTDIR";
  Config.base_cfg.verbose <- false;
  let ds = Input.parse dpt_file in
  let renaming, ds = FrontendPipeline.process_prog Builtins.interp_builtin_tys ds in
  let ds = MidendPipeline.process_prog ds in
  let pp, _ = Preprocess.preprocess ds in
  if not (InterpTopo.contains_topology !spec_file)
  then fail "spec must have a topology block (with a single node)";
  let spec, topo = InterpTopo.parse pp renaming !spec_file in
  check_single_node topo;
  let sorts = Collections.CidMap.map (fun (sort, _, _) -> sort) pp.events in
  let wire ev = InterpDeparsing.event_wire_bytes sorts ev in
  (* inputs: group by port, keep time order (stable for equal times) *)
  let inputs =
    List.map
      (function
        | IEvent { iev; ilocs; itime } -> input_port ilocs, (itime, wire iev)
        | IControl _ ->
          fail "control commands are not supported (the C backend has no control plane)")
      spec.events
  in
  let ports = List.map fst inputs |> List.sort_uniq compare in
  IoUtils.ensure_dir !out_dir;
  List.iter
    (fun port ->
      let records =
        List.filter_map (fun (p, r) -> if p = port then Some r else None) inputs
        |> List.stable_sort (fun (t1, _) (t2, _) -> compare t1 t2)
      in
      write_pcap (Filename.concat !out_dir (Printf.sprintf "in_%d.pcap" port)) records)
    ports;
  (* expected outputs: parse each structured event with the spec's own event
     parser, located at its port; raw {"bytes": hex} entries pass through *)
  let json = Yojson.Basic.from_file !spec_file in
  match Yojson.Basic.Util.member "expected" json with
  | `Null -> ()
  | `Assoc per_port ->
    let encode_entry port (entry : Yojson.Basic.t) =
      match entry with
      (* case: structured event -> depase *)
      | `Assoc fields when List.mem_assoc "name" fields ->
        let located =
          `Assoc
            (List.filter (fun (k, _) -> k = "name" || k = "args") fields
             @ [ "locations", `List [ `String (Printf.sprintf "0:%d" port) ]
               ; "timestamp", `Int 0 ])
        in
        (match InterpSpec.parse_interp_inputs pp renaming 1 1 [ located ] with
         | [ IEvent { iev; _ } ] ->
           `Assoc (event_to_json iev @ [ "wire_bytes", `String (hex (wire iev)) ])
         | _ -> fail "bad expected event")
      (* case: unparsed packet -> check its wellformed and copy over *)
      | `Assoc [ ("bytes", `String b) ] ->
        let b = String.lowercase_ascii b in
        if String.length b mod 2 <> 0
           || not (String.for_all (fun c -> String.contains "0123456789abcdef" c) b)
        then fail ("expected bytes must be an even-length hex string: " ^ b);
        `Assoc [ "wire_bytes", `String b ]
      | e ->
        fail
          ("an expected output is {\"name\": ..., \"args\": [...]} or {\"bytes\": hex}, not "
           ^ Yojson.Basic.to_string e)
    in
    let encoded =
      List.map
        (fun (port_str, entries) ->
          let port =
            try int_of_string port_str with
            | _ -> fail ("expected: port must be a number, not " ^ port_str)
          in
          match entries with
          | `List entries -> port_str, `List (List.map (encode_entry port) entries)
          | _ -> fail ("expected: port " ^ port_str ^ " must map to a list"))
        per_port
    in
    Yojson.Basic.to_file
      ~std:true
      (Filename.concat !out_dir "expected.json")
      (`Assoc encoded)
  | _ -> fail "\"expected\" must map ports to lists of outputs"
;;

let _ =
  try main () with
  | CoreSyntax.Error msg -> fail msg
;;

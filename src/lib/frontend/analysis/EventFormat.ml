open Syntax
open SyntaxUtils
open Batteries
open Collections
open Printing
(* 
  This file prints the expected wire format of an event value.
  If a compiler or interpreter generates an implementation that 
  doesn't match the expected format, it is wrong. 
*)

(* set integer id numbers for all event declarations *)
let set_event_nums ds =
  let num = ref 1 in
  let used_nums = ref [0] in
  let find_used_nums = 
    object(_)
      inherit [_] s_iter
      method !visit_DEvent _ _ num_opt _ _ _ =
        match num_opt with
        | Some n -> 
          used_nums := n::(!used_nums);
        | _ -> ();
    end
  in
  find_used_nums#visit_decls () ds;
  let v = 
    object(_)
      inherit [_] s_map
      method !visit_DEvent _ id num_opt sort specs args =
        match num_opt with
        | None -> 
          (* increment num until find something not in used nums *)
          while List.mem !num (!used_nums) do
            num := !num + 1
          done;
          let rv = DEvent(id, Some !num, sort, specs, args) in
          num := !num + 1;
          rv
        | Some _ -> DEvent(id, num_opt, sort, specs, args)
    end
  in
  v#visit_decls () ds
;;


(* print the hex representation of a value *)
let int_to_hex bitwidth intval =
  if bitwidth mod 8 != 0 then
    failwith "Bitwidth must be divisible by 8"
  else
    let hex = Printf.sprintf "%x" intval in
    let padding = String.make ((bitwidth / 4) - String.length hex) '0' in
    padding ^ hex
;;
(* get the fixed header for an event *)
let lucid_header_hexstr evnum = 
  (* hex string of an ethernet header with etype 029A, followed by a 16-bit 
     big-endian representation of the int evnum *)
  "000000000001000000000002029A"^(int_to_hex 16 evnum)
;;

let rec event_to_header_and_params cid decls = 
  match decls with
  | [] -> error "[event_to_header] event not found"
  | ({d = DEvent(id, num_opt, ev_sort, _, params)})::ds -> (
    if Id.equal (Cid.to_id cid) id then
      match num_opt with
      | None -> error "[event_to_header] event has no number"
      | Some n -> (
        if (ev_sort = EBackground) then
          lucid_header_hexstr n, params
        else
          (* there is no header for a packet event *)
          "", params
      )
    else
      event_to_header_and_params cid ds
  )
  | _::ds -> event_to_header_and_params cid ds
;;

let rec value_ty_to_hex ds value ty = 
  match value.v with 
  | VBool true -> "01"
  | VBool false -> "00"
  | VInt (zint) -> 
    let bitwidth = match ty.raw_ty with 
      | TInt sz -> extract_size sz
      | _ -> error "[value_to_sized_hex] not an int type"
    in
    let intval = Integer.to_int zint in
    int_to_hex bitwidth intval
  | VGlobal _ -> error "[value_to_hex] cannot convert a global to a hex"
  | VGroup _ -> error "[value_to_hex] cannot convert a group to a hex"
  | VPat _ -> error "[value_to_hex] cannot convert a pattern to a hex"
  | VEvent{eid; data} -> 
    let header, params = event_to_header_and_params eid ds in
    let data_hex = List.map2 (fun v p -> value_ty_to_hex ds v (snd p)) data params in
    header ^ (String.concat "" data_hex)
;;

let exp_ty_to_hex ds exp ty = 
  match exp.e with 
  | EVal v -> value_ty_to_hex ds v ty
  | EInt(z, _) -> 
    let bitwidth = match ty.raw_ty with 
      | TInt sz -> extract_size sz
      | _ -> error "[exp_ty_to_hex] not an int type"
    in
    let intval = Z.to_int z in
    int_to_hex bitwidth intval
  | _ -> error "[exp_ty_to_hex] not a value"

let event_to_hex ds str = 
  let str = "const some_ty some_var = "^str^";" in
  let decls = 
    Input.parse_from_string@@str
  in
  match (List.hd decls).d with 
  | DConst(_, _, ev_exp) -> (
    match ev_exp.e with 
    | ECall(ev_cid, args, _) -> 
      let header, params = event_to_header_and_params ev_cid ds in
      let arg_strings = List.map2 (fun arg param -> exp_ty_to_hex ds arg (snd param)) args params in
      header ^ (String.concat "" arg_strings)
      (* value_ty_to_hex ds ev_val (ty TEvent) *)
    | _ -> error "[serialize_event_value] input parsing error "
  )
  | _ -> error "[serialize_event_value] input parsing error "  
;;




let test () = 
  let testprog = "packet event foo@0x1(int x, int y);" in
  let ds = Input.parse_from_string testprog in
  print_endline("Hex value of packet event foo(0xffffffff, 0xfffffffe)");
  print_endline (event_to_hex ds "foo(0xffffffff, 0xfffffffe)");
;;

(* deparser implementation *)
(* notes 
    - Payload is a flag (carry input payload or not)
*)
open CCoreSyntax
open CCoreExceptions
let bytes_t = CCoreParse.bytes_t


(*  macros *)
let memcpy dst_ref_exp src_exp = 
  sassign_exp 
    (ederef (ecast (tref src_exp.ety) dst_ref_exp))
    src_exp
;;
let memset dst_ref_exp src_val = 
  sassign_exp 
    (ederef (ecast (tref src_val.vty) dst_ref_exp))
    (eval src_val)
;;
(* let memset_word dst_ref_exp src_val = 
  ((ecast (tref src_val.vty) dst_ref_exp))
;; *)
let memset_n dst_ref_exp src_val n = 
  stmts 
    (List.init n
      (fun i -> 
        let idx_exp = eval@@vint i 32 in
        ((ecast (tref src_val.vty) dst_ref_exp), idx_exp)/<-(eval src_val)))
;;
let ptr_incr eptr i = (eptr/+(eval@@vint i 32))
let sptr_incr eptr i = sassign_exp eptr (ptr_incr eptr i)
;;
(* 
  // bytes refs an empty buffer that is large enough
  // event parameters are efficiently packable (no padding)
  void deparse_event(event* ev_out, bytes_t* bytes) {
    if (ev_out->is_packet == 1) {
      *(())
      // empty eth header
      memset(bytes->cur, 0, 14);
      *(bytes->cur) = *(bytes->cur) + 14;
      // event tag
      *((uint16_t* )(bytes->cur)) = event->tag;
      *(bytes->cur) = *(bytes->cur) + 2;
    }
    // event data
    *((event_data_t* ) bytes->cur) = event->data;
    *(bytes->cur) = *(bytes->cur) + sizeof(event->data);
    return
  }
*)
(* find a type definition based on its id *)
let rec find_ty_opt ty_cid decls = 
  match decls with 
  | [] -> None
  | decl::decls -> (
    match decl.d with 
    | DTy(cid, Some(ty)) -> 
      if (Cid.equal cid ty_cid) then Some(ty) else (find_ty_opt ty_cid decls)
    | _ -> find_ty_opt ty_cid decls
  )
;;

let deparse_fun event_t = 
  let tag_enum_ty = 
    extract_trecord_or_union (base_type event_t) |> snd
    |> List.hd (* event.tag *)
  in
  let data_union_ty = List.nth
    (extract_trecord_or_union (base_type event_t) |> snd) 
    1 (* event.data *)
  in
  (* print_endline ("tag_enum_ty: "^(CCorePPrint.ty_to_string ~use_abstract_name:true tag_enum_ty));
  print_endline ("data_union_ty: "^(CCorePPrint.ty_to_string data_union_ty)); *)
  let tag_cids = List.split (extract_tenum (base_type tag_enum_ty)) |> fst in
  let event_struct_tys = extract_trecord_or_union (base_type data_union_ty) |> snd in
  (* let tag_symbols = List.map (fun cid -> eval@@vsymbol cid tag_enum_ty) tag_cids in *)
  let ev_out_param = id"ev_out", tref event_t in
  let buf_out_param = id"buf_out", tref bytes_t in
  let ev_out = param_evar ev_out_param in
  let buf_out = param_evar buf_out_param in
  (* start function def *)
  let fun_id = id"deparse_event" in
  let params = [ev_out_param; buf_out_param] in
  let body = stmts [
    sif (eop Eq [(ev_out/->id"is_packet");(eval@@vint 0 8)])
      (* not a packet / raw event *)
      (stmts [
        (* set empty eth header *)
        memset_n (buf_out/->id"cur") (vint 0 32) 3; (* 12 bytes *)
        memset_n (ptr_incr (buf_out/->id"cur") 12) (vint 0 16) 1; (* 2 bytes *)
        sptr_incr (buf_out/->id"cur") 14;
        (* set event type *)
        memcpy (buf_out/->id"cur") (ev_out/->id"tag");
        sptr_incr (buf_out/->id"cur") 2;
      ])
      (snoop);
    smatch [ev_out/->id"tag"] 
      (* one branch for each event *)
      (List.map2 
        (fun tag_cid event_struct_ty -> 
          let this_event_data_field = CCoreEvents.event_untag (Cid.to_id tag_cid) in
          [PVal(vsymbol tag_cid tag_enum_ty)],
          stmts [
            (* copy to memory *)
            memcpy (buf_out/->id"cur") ((ev_out/->id"data")/.this_event_data_field);
            (* increment pointer *)
            sptr_incr (buf_out/->id"cur") (size_of_ty event_struct_ty)
          ])
        tag_cids
        event_struct_tys)]
  in
  dfun (Cid.id fun_id) tunit params body
;;

(* find the event type definition and put the deparser 
   right after it.
   The event type definition should appear after everything 
   it depends on.
   It would also be safe to put the deparser at the end, since its 
   not called by anything except the handler, as long as that's not already 
   implemented by this point in the compilation. *)
let rec process_inner decls = match decls with 
  | [] -> []
  | decl::decls -> (
    match decl.d with 
    | DTy(cid, Some(ty)) when Cid.equal cid (Cid.id (CCoreEvents.event_tunion_tyid)) -> 
      let event_t = tabstract_cid cid ty in
      let deparse_decl = deparse_fun event_t in
      decl::(deparse_decl::decls)
    | _ -> decl::(process_inner decls)
  )

let process decls = 
  process_inner decls
;;
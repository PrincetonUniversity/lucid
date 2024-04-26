(* deparser implementation *)
(* 

Assumptions:
  - the packet_t buffer's "payload" pointer points to the payload 
    of the packet that we wish to send out.  
  - event parameters are efficiently packable (no padding)
  - the packet_t buffer has sufficient headroom before "start" to 
    store any event that can be serialized, even if it is larger 
    than the event that was extracted from the packet upon arrival.

deparse_event function:
  1. calculate the new start: packet->payload - event->len
  2. if the event is a background event, 
     prepend an ethernet header before start and the 
     16-bit event tag.
  3. copy the event's data to the start of the packet.

example:
program with an event 
  do_add(int i, int j, int k);
==>
fn int deparse_event(event* ev_out, packet_t* buf_out){
  if (ev_out->is_packet == 0) {
    ((int* )(buf_out->cur))[0] = 0;
  ((int* )(buf_out->cur))[1] = 0;
  ((int* )(buf_out->cur))[2] = 0;
  ((int16* )(buf_out->cur + 12))[0] = 0;
  buf_out->cur = buf_out->cur + 14;
  (*(((event_tag_t*)(buf_out->cur)))) = ev_out->tag;
  buf_out->cur = buf_out->cur + 2;
} else {
  skip;
}
match (ev_out->tag) {
  case do_add_tag: {
    (*(((do_add_t*)(buf_out->cur)))) = ev_out->data.do_add;
    buf_out->cur = buf_out->cur + 12;
  }
}
return (buf_out->cur) - (buf_out->start);
}

TODO: 
  - support payloads
  - in-place deparsing vs packet copying
*)
open CCoreSyntax
open CCoreExceptions
let packet_t = CCoreParse.packet_t


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


let deparse_fun event_defs event_t = 
  let ev_out_param = cid"ev_out", tref event_t in
  let buf_out_param = cid"buf_out", tref packet_t in
  let ev_out = param_evar ev_out_param in
  let buf_out = param_evar buf_out_param in
  (* start function def *)
  let fun_id = id"deparse_event" in
  let params = [ev_out_param; buf_out_param] in
  let body = stmts [
    (* calculate new start *)
    sassign_exp (buf_out/->cid"start") ((buf_out/->cid"start")/-((ev_out/->cid"len")));
    (* if not a packet, prepend ethernet header and event tag *)
    sif (eop Eq [(ev_out/->cid"is_packet");(eval@@vint 0 8)])
      (* not a packet / raw event *)
      (stmts [
        (* prepend eth header and event tag *)
        sassign_exp (buf_out/->cid"start") ((buf_out/->cid"start")/-((eval@@vint 16 32)));
        memset_n (buf_out/->cid"start") (vint 0 32) 3; (* 12 bytes *)
        memset_n (ptr_incr (buf_out/->cid"start") 12) (vint 0 16) 1; (* 2 bytes *)
        (* set event type *)
        memcpy (ptr_incr (buf_out/->cid"start") 14) (ev_out/->cid"tag");
        (* update start -- should now be back to before prepend *)
        sptr_incr (buf_out/->cid"start") 16;
      ])
      (snoop);
    smatch [ev_out/->cid"tag"] 
      (* now branch on tag, copying event parameters to packet *)
      (List.map
        (fun event_def -> 
          let event_data_ty = CCoreEvents.event_param_ty event_def in
          [PVal(vint (Option.get (event_def.evconstrnum)) event_tag_size)],
          stmts [
            (* this is wrong -- data is the size of the largest event. 
                We need to cast ev_out to a pointer to the event's parameter type *)
      (*this is what we want ( *(((do_add_t* )(buf_out->start)))) = *((do_add_t* )(ev_out)); *)
          sassign_exp 
            (ederef (ecast (tref event_data_ty) (buf_out/->cid"start")))
            (ederef (ecast (tref event_data_ty) (ev_out)));
          (* memcpy (buf_out/->cid"start") (ecast (tref event_data_ty) (ev_out)); *)
            (* increment pointer -- this is optional, but should take it back to payload *)
            sptr_incr (buf_out/->cid"start") (CCoreEvents.event_len event_def);
        ])
        event_defs);
    ]
  in
  dfun (Cid.id fun_id) (tunit) params body
;;

(* find the event type definition and put the deparser 
   right after it.
   The event type definition should appear after everything 
   it depends on.
   It would also be safe to put the deparser at the end, since its 
   not called by anything except the handler, as long as that's not already 
   implemented by this point in the compilation. *)
let rec process_inner event_defs decls = match decls with 
  | [] -> []
  | decl::decls -> (
    match decl.d with 
    | DEvent _ -> process_inner event_defs decls (* events are not needed after this, so remove *)
    | DTy(cid, Some(ty)) when Cid.equal cid ((CCoreEvents.event_ty_id)) -> 
      let event_t = tabstract_cid cid ty in
      let deparse_decl = deparse_fun event_defs event_t in
      decl::(deparse_decl::decls)
    | _ -> decl::(process_inner event_defs decls)
  )

let process decls = 
  let event_defs = List.filter_map (extract_devent_opt) decls in 
  process_inner event_defs decls
;;
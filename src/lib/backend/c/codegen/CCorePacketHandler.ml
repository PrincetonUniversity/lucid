(* implementation of the toplevel packet handler function *)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils
(* the packet handler is a fixed function that uses the compiler generated functions: 
    - parse_event
    - handle_event
    - deparse_event
*)
let get_event_tag t_event = 
  let ev_param = cid"ev", tref t_event in
  dfun 
    (cid"get_event_tag")
    (tint event_tag_size)
    [ev_param]
    (sret (ecast (tint event_tag_size) ((param_evar ev_param)/->cid"tag")))
;;
let reset_event_tag t_event = 
  (* this isn't right. Need an address.. *)
  let ev_param = cid"ev", tref t_event in
  let enum_ty = ((param_evar ev_param)/->cid"tag").ety in 
  dfun 
    (cid"reset_event_tag")
    (tunit)
    [ev_param]
    (sassign_exp ((param_evar ev_param)/->cid"tag") (ecast (enum_ty) (default_exp (tint event_tag_size))))
;;

let reset_cursor = 
  let buf_param = cid"buf", tref tchar in
  let len_param = cid"len", tint 32 in
  let bs_param = cid"bytes", tref CCoreParse.bytes_t in
  let buf = param_evar buf_param in
  let bs = param_evar bs_param in
  let len = param_evar len_param in
  dfun 
    (cid"reset_cursor")
    tunit
    [buf_param; len_param; bs_param]    
    (stmts [
      sassign_exp (bs/->cid"start") (buf);
      sassign_exp (bs/->cid"cur") (buf);
      sassign_exp (bs/->cid"end") (buf/+len)])
;;

let pkt_handler_str = {| 
{
    // fixed-function toplevel packet handler 
    // locals (that should maybe be globals?)
    bytes_t bytes_v;
    event_t ev1_v;
    event_t ev2_v;
    event_t ev_out_v;

    bytes_t * bytes = &bytes_v;
    event_t * ev1 = &ev1_v;
    event_t * ev2 = &ev2_v;
    event_t * ev_out = &ev_out_v;
    event_t * ev_tmp;
    int generated_port = 0; // return value

    // prepare the cursor
    reset_cursor(buf, len, bytes);
    // parse the event
    uint8_t parse_success = parse_event(bytes, ev1);
    if (parse_success == 1) {
        // event continuation trampoline
        for (int i=0; i < 100; i++) {
            reset_event_tag(ev2);
            generated_port = handle_event(ingress_port, ev1, ev2, ev_out);
            // we have generated an event to ev2. 
            // We want to make ev1 point to that, and ev2 point to the old ev1, 
            // so that we can continue processing the chain.
            if (get_event_tag(ev1) != 0) {
                ev_tmp = ev1;
                ev1 = ev2; 
                ev2 = ev_tmp;
            }
            // no more generated events. Goto output.
            else {
                break;
            }
        }
        // we have generated an output event, write it to the buffer
        if (generated_port != 0) {
            *out_len = deparse_event(ev_out, bytes);
        }
    }
    // return the port that the event goes out on, 
    // 0 means "nothing"
    return generated_port;
}
|}
;;
let pkt_handler = dfun_foriegn 
  (cid"pkt_handler")
  (tint 32)
  [
    cid"ingress_port", tint (!CCoreConfig.cfg).port_id_size;
    cid"buf", tref tchar; cid"len", tint 32; 
    cid"out_buf", tref tchar; cid"out_len", tref@@tint 32; 
  ]
  pkt_handler_str
;;
let main_fun = 
  dforiegn 
{|int main(int argc, char const *argv[])
  {
    /* code */
    return 0;
  }|}
let process decls = 
  let t_event = match (find_ty_opt (Cid.id CCoreEvents.event_tunion_tyid) decls) with 
    | Some(ty) -> ty
    | _ -> err "no tevent"
  in
  decls@[get_event_tag t_event; reset_event_tag t_event; reset_cursor; pkt_handler]@[main_fun]
;;

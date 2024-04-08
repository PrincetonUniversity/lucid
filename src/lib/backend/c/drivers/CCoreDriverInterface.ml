open CCoreSyntax
open CCoreExceptions
open CCoreUtils

(* a toplevel driver declares some helpers 
   that it derives from the program, 
   some import statements, 
   a packet handler, 
   and a main function *)
module type DriverInterface = sig
   val imports : decl list
   val helpers : decl list -> decl list
   val pkt_handler : decl
   val main : decl
   val cflags : string
end

let package (module D : DriverInterface) decls = 
   let imports = D.imports in
   let helpers = D.helpers decls in
   let pkt_handler = D.pkt_handler in
   let main = D.main in
   imports @ helpers @ [pkt_handler; main], D.cflags
;;

(* default helpers *)
let get_event_tag t_event = 
   let ev_param = id"ev", tref t_event in
   dfun 
     (cid"get_event_tag")
     (tint event_tag_size)
     [ev_param]
     (sret (ecast (tint event_tag_size) ((param_evar ev_param)/->id"tag")))
 ;;
 let reset_event_tag t_event = 
   (* this isn't right. Need an address.. *)
   let ev_param = id"ev", tref t_event in
   let enum_ty = ((param_evar ev_param)/->id"tag").ety in 
   dfun 
     (cid"reset_event_tag")
     (tunit)
     [ev_param]
     (sassign_exp ((param_evar ev_param)/->id"tag") (ecast (enum_ty) (default_exp (tint event_tag_size))))
 ;;
 
 let reset_cursor = 
   let buf_param = id"buf", tref tchar in
   let len_param = id"len", tint 32 in
   let bs_param = id"bytes", tref CCoreParse.bytes_t in
   let buf = param_evar buf_param in
   let bs = param_evar bs_param in
   let len = param_evar len_param in
   dfun 
     (cid"reset_cursor")
     tunit
     [buf_param; len_param; bs_param]    
     (stmts [
       sassign_exp (bs/->id"start") (buf);
       sassign_exp (bs/->id"cur") (buf);
       sassign_exp (bs/->id"end") (buf/+len)])
 ;;
 
 let default_helpers decls = 
   let teventstruct = match (find_ty_opt (Cid.id CCoreEvents.event_tunion_tyid) decls) with 
      | Some(ty) -> ty
      | _ -> err "no tevent"
   in  
   [
      get_event_tag teventstruct;
      reset_event_tag teventstruct;
      reset_cursor
   ]
;;

let default_imports = [
   CCoreSyntax.dinclude "<stdio.h>";
   CCoreSyntax.dinclude "<stdlib.h>";
   CCoreSyntax.dinclude "<stdint.h>"
];;
    
let default_pkt_handler = dfun_foriegn 
   (cid"pkt_handler")
   (tint 32)
   [
     id"ingress_port", tint (!CCoreConfig.cfg).port_id_size;
     id"buf", tref tchar; id"len", tint 32; 
     id"out_buf", tref tchar; id"out_len", tref@@tint 32; 
   ]
   {| 
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
   }|}   
;;
 
let default_cflags = ""
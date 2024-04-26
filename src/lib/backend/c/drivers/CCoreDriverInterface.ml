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
   imports @ decls @ helpers @ [pkt_handler; main], D.cflags
;;

(* default helpers *)
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
 
 let init_cursor = 
   let bytes_ptr_t = tref CCoreParse.bytes_t |> CCorePPrint.ty_to_string ~use_abstract_name:true in
   dforiegn [%string
{|
void init_cursor(char*  buf , uint32_t len , %{bytes_ptr_t}  bytes ){
   bytes->start = buf;
   bytes->payload = buf;
   bytes->end = buf + len;
}|}]
;;
let reset_cursor = 
   let bytes_ptr_t = tref CCoreParse.bytes_t |> CCorePPrint.ty_to_string ~use_abstract_name:true in
   dforiegn [%string
{|
void reset_cursor(%{bytes_ptr_t}  bytes){
   bytes->payload = bytes->start;
}|}]
let copy_payload = 
   dforiegn [%string
{|
void copy_payload(bytes_t*  buf_out , bytes_t*  buf_in ) {
   memcpy(buf_out->payload, buf_in->payload, buf_in->end - buf_in->payload);
   buf_out->payload = buf_out->payload + (buf_in->end - buf_in->payload);
}|}]    
;;

 let default_helpers decls = 
   let teventstruct = match (find_ty_opt (CCoreEvents.event_ty_id) decls) with 
      | Some(ty) -> ty
      | _ -> err "no tevent"
   in  
   [
      get_event_tag teventstruct;
      reset_event_tag teventstruct;
      init_cursor;
      reset_cursor;
      copy_payload;
   ]
;;

let default_imports = [
   CCoreSyntax.dinclude "<stdio.h>";
   CCoreSyntax.dinclude "<stdlib.h>";
   CCoreSyntax.dinclude "<stdint.h>";
   CCoreSyntax.dinclude "<stdbool.h>";
];;
    
let default_pkt_handler = dforiegn [%string
{|
uint8_t pkt_handler(uint8_t ingress_port, bytes_t* pkt_in, bytes_t* pkt_out){
   // locals
   event_t ev1_v = {0};
   event_t ev2_v = {0};
   event_t ev_out_v = {0};
   int generated_port = 0; // return value

   event_t * ev1 = &ev1_v;
   event_t * ev2 = &ev2_v;
   event_t * ev_out = &ev_out_v;
   event_t * ev_tmp;
   uint8_t parse_success = parse_event(pkt_in, ev1);       
   if (parse_success == 1) {
      // event continuation trampoline
      #pragma unroll 4
      for (int i=0; i < 100; i++) {
         reset_event_tag(ev2);
         generated_port = handle_event(ingress_port, ev1, ev2, ev_out);
         if (get_event_tag(ev2) == 0) {
            break;
         }
         ev_tmp = ev1;
         ev1 = ev2; 
         ev2 = ev_tmp;
      }
      // we have generated an output event, write it to the buffer
      if (generated_port != 0) {
            deparse_event(ev_out, pkt_out);
            copy_payload(pkt_out, pkt_in);
      }
   }
   else {
      debug_printf ("parse failed!\n");
      exit(1);
   }
   return generated_port;
}|}]
   

let default_cflags = ""
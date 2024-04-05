(* implementation of the toplevel packet handler function *)
open CCoreSyntax
open CCoreExceptions
open CCoreUtils
(* the packet handler is a fixed function that uses the compiler generated functions: 
    - parse_event
    - handle_event
    - deparse_event
*)

(* find a type definition based on its id *)
let rec find_ty_opt ty_cid decls = 
  match decls with 
  | [] -> None
  | decl::decls -> (
    match decl.d with 
    | DTy(cid, Some(ty)) -> 
      if (Cid.equal cid ty_cid) then Some(tabstract_cid cid ty) else (find_ty_opt ty_cid decls)
    | _ -> find_ty_opt ty_cid decls
  )
;;

let get_event_tag t_event = 
  let ev_param = id"ev", tref t_event in
  dfun 
    (cid"get_event_tag")
    (tint event_tag_size)
    [ev_param]
    (
      sret 
        (
          ecast (tint event_tag_size) ((param_evar ev_param)/->id"tag")
        )
    )
;;

let reset_event_tag t_event = 
  (* this isn't right. Need an address.. *)
  let ev_param = id"ev", tref t_event in
  let enum_ty = ((param_evar ev_param)/->id"tag").ety in 
  dfun 
    (cid"reset_event_tag")
    (tunit)
    [ev_param]
    (sassign_exp 
      ((param_evar ev_param)/->id"tag") 
      
      (ecast (enum_ty) (default_exp (tint event_tag_size)) )
    )
;;

let bytes_t = CCoreParse.bytes_t

let reset_cursor = 
  let buf_param = id"buf", tref tchar in
  let len_param = id"len", tint 32 in
  let bs_param = id"bytes", tref bytes_t in
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

let pkt_handler_str = {| 
// fixed toplevel packet handler 
int pkt_handler(char* buf, int len, char* out_buf, int* out_len) {
    int generated_port = 0;
    // prepare the cursor
    reset_cursor(buf, len, bytes);
    uint8_t parse_success = parse_event(bytes, ev1);
    if (parse_success == 1) {
        for (int i=0; i < 100; i++) {
            set_event_tag(ev2, 0);
            generated_port = handle_event(ev1, ev2, ev_out);
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
    id"buf", tref tchar; id"len", tint 32; 
    id"out_buf", tref tchar; id"out_len", tref@@tint 32; 
  ]
  pkt_handler_str


;;

let process decls = 
  let t_event = match (find_ty_opt (Cid.id CCoreEvents.event_tunion_tyid) decls) with 
    | Some(ty) -> ty
    | _ -> err "no tevent"
  in
  decls@[get_event_tag t_event; reset_event_tag t_event; reset_cursor; pkt_handler]
;;


(*  
copy and pasted from packet handler to make it check 
without any compiler generated code.
/* uncomment this is you want the gcc syntax checker
to pass before compilation is finished.  */
/* 
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct bytes_t bytes_t;
typedef struct event_t event_t;

extern bytes_t bytes_v;
extern bytes_t* bytes;

extern event_t ev1_v;
extern event_t ev2_v;
extern event_t ev_out_v;

extern event_t* ev1;
extern event_t* ev2;
extern event_t* ev_out;
extern event_t* ev_tmp;

extern int deparse_event(event_t* ev_out, bytes_t* buf_out);
extern uint8_t parse_event(bytes_t* pkt, event_t* ev_in);
extern int handle_event(event_t* ev_in, event_t* ev_next, event_t* ev_out);

extern int get_event_tag(event_t* ev);
extern void set_event_tag(event_t* ev, int tag);
extern void reset_cursor(char* buf, int len, bytes_t* bytes);
*/
   


*)
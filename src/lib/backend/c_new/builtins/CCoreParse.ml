(* implementation of parse functions *)


open CCoreSyntax

(* parsing methods implemented by this module *)
let read_cid = Cid.create ["parse"; "read"] ;;
let peek_cid = Cid.create ["parse"; "peek"] ;;
let skip_cid = Cid.create ["parse"; "skip"] ;;
let drop_cid = Cid.create ["parse"; "drop"] ;;

(* payload parsing is inlined into generate? *)
let payload_parse = Cid.create ["Payload"; "parse"] ;;

(* then I think there's a call too?  *)

(* and the payload or packet type *)
(* and hash / crc *)
(* and then deparsing / serialization too *)
(* maybe do_lucid_parsing? Not sure. *)
let process_parser decl = 
  let id, ty, params, body = extract_dparser decl in
  let _, _, _, _ = id, ty, params, body in
  decl
;;

(* 
input: 
parser void main(bit[1500] pkt){
  parse_skip();
  int16 tag = parse_read(pkt);
  match (tag) {
    case 1: {
      int a = parse_read(pkt);
      int b = parse_read(pkt);
      generate_self(foo(a, b)) /* extern call */;
    }
    case 2: {
      int c = parse_read(pkt);
      generate_self(bar(c)) /* extern call */;
    }
    case 3: {
      int d = parse_read(pkt);
      generate_self(baz(d)) /* extern call */;
    }
    case 4: {
      int i = parse_read(pkt);
      int j = parse_read(pkt);
      generate_self(main(i, j)) /* extern call */;
    }
  }
}   


transformations: 
  1. make skip take an argument (pkt)
  2.  

*)

let process fds = 

  
  fds 

;;

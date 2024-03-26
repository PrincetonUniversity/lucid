(* implementation of parse functions *)

(* 
Design:  

  The bytes struct
  - this represents unparsed packets
    typedef struct bytes_t {
      char* start;  // pointer to start of buffer
      char* cur;    // pointer to current position
      char* end;    // pointer to end of buffer
    } bytes_t;

skip, peek, and read are methods that operate on the packet buffer. 
One method is generated for each type. 

void skip_int(bytes_t* bs) {
    bs->cur = bs->cur + sizeof(int);
}

int peek_int(bytes_t* bs) {
 // EOp(Cast(TRef(TInt(32))), [EProj( EDeref(EVar("bs")), "cur")])
    return ((int * ) (bs->cur))[0];
}

int read_int(bytes_t* bs) {
    int rv = ((int * ) (bs->cur))[0];
    skip_int(bs);
    return rv;
} 

In the parser functions, bytes_t* bs is an argument

For deparsing (which is not in this module)
  TODO: list semantics. 
    - Payload is a flag (carry input payload or not)
    - the function that writes out the packet will get a 
      value of the event and the bytes_t of the packet. 
         - what happens next is platform dependent. 
          - in ebpf, it will shrink or grow the buffer of the 
            current packet to hold the new event. 
          - in userspace c it will likely allocate 
            output buffer and copy the event header and payload there. 
*)


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
  print_endline ("implementing parser functions");
  exit 1;

  fds 

;;

type cid = Cid.t
type tcid = TaggedCid.t
type t =
  { fname : string
  ; start : int
  ; finish : int
  ; spid : int
  ; global_created_in_src : tcid option (* name of the global created in program src, if any*)
  }
(* [@@deriving show, ord] *)

let default = { fname = ""; start = -1; finish = -1; spid = -1; global_created_in_src =  None;}

let create fname start finish = {default with fname; start; finish}
;;

let extend (x : t) (y : t) : t =
  assert (x.fname = y.fname);
  let s = min x.start y.start in
  let f = max x.finish y.finish in
  { x with start = s; finish = f }
;;


let to_string (span : t) =
  if span = default
  then "default_span"
  else Printf.sprintf "%s:(%d,%d)" span.fname span.start span.finish
;;

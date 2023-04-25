type id = Id.t
type cid = Cid.t
type indexed_id = (id * int)

type t = 
| IId of indexed_id
| ICid of indexed_id * t

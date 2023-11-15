include LibraryInterface.TypeInterface

(* These functions are weird, so sometimes we need to reference them directly *)
val payload_parse_cid : Cid.t
val payload_empty_cid : Cid.t
val payload_read_cid  : Cid.t
val payload_peek_cid  : Cid.t
val payload_ty        : Syntax.ty

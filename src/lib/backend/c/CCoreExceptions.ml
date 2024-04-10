open CCoreSyntax
open CCorePPrint
exception TypeMismatch of ty * ty
exception UnboundVariable of cid
exception LengthMismatch of int * int
exception UnboundField of id
exception InvalidSize of size
exception UnboundType of cid
exception SizeMismatch of size * size
exception SelfRefSize of size
exception TypeError of string

(* exceptions *)
exception CCoreError of string
exception CCoreTy of ty
let err str = raise(CCoreError(str));;

let ty_err str = raise(TypeError(str));;
let err_expected_ty ty str = 
  raise(TypeError("["^(ty_to_string ~use_abstract_name:true ty^"] "^str)))
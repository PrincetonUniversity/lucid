(* CCoreMaskWidths: keep non-standard-width unsigned ints in range.

   An int<n> whose width n is not 8/16/32/64 is stored internally 
   in a standard-width container (uint8/16/32/64). 
   To provide arbitrary-width semantics, this pass masks the result of 
   every operation on a nonstandard int<n> with BitAnd(_, (1<<n)-1).

   It runs late (after handler/variant lowering, while Read/Peek are still ops),
   so it also covers the packet-read entry point: a read of int<n> memcpys
   ceil(n/8) bytes, so the last byte carries the neighbour's high bits.

   Masked (width-violating producers):
     Plus, Sub, Neg, LShift, BitNot, Cast _, Hash _, Read _, Peek _.
   Eliminated here (no C lowering; result is naturally masked):
     Slice(hi,lo) -> (a >> lo) & ((1<<(hi-lo+1))-1).
*)
open CCoreSyntax

let is_standard n = List.mem n [8; 16; 32; 64]

(* the low-n-bit mask as an int<n> constant. OCaml's native int is 63-bit, so
   (1 lsl n)-1 is only exact for n <= 61; the wider non-standard widths (62, 63)
   are left unmasked for now -- they sit in the deferred ">= 62-bit" bucket along
   with >64. *)
let maskable n = (not (is_standard n)) && n <= 61

let mask_const n = eval (vint ((1 lsl n) - 1) n)
let mask_to n e = eop BitAnd [e; mask_const n]   (* result type = e.ety = int<n> *)

(* ops whose result can carry bits >= n *)
let is_violating_op = function
  | Plus | Sub | Neg | LShift | BitNot | Cast _ | Hash _ | Read _ | Peek _ -> true
  | _ -> false

let masker = object (_)
  inherit [_] s_map as super
  method! visit_exp () e =
    let e = super#visit_exp () e in   (* bottom-up: operands already masked *)
    match e.ety.raw_ty, e.e with
    (* Slice -> (a >> lo) & mask : eliminate the op (it has no C form); the mask
       is the slice's own width, so the result is clean by construction. *)
    | TInt n, EOp(Slice(_, lo), [a]) ->
        let shifted = if lo = 0 then a else eop RShift [a; eval (vint lo 32)] in
        let res = if maskable n then mask_to n shifted else shifted in
        { res with ety = e.ety }   (* preserve the slice's int<n> result type *)
    (* width-violating producer with a non-standard result -> mask it *)
    | TInt n, EOp(op, _) when maskable n && is_violating_op op -> mask_to n e
    | _ -> e
end

let process decls = masker#visit_decls () decls
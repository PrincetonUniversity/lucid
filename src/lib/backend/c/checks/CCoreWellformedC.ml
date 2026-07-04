(* misc checks to make sure constructs not supported in 
   C have been eliminated.  *)

open CCoreSyntax
open CCoreExceptions


(*** Bit ints ***)
(* Bit ints (ints that are not 8, 16, 32, or 64 bits) are represented as their
   smallest standard container (uint8/16/32/64) everywhere -- locals, params,
   returns, fields -- and kept in range by the mask-insertion pass
   (CCoreMaskWidths). So they are NOT restricted to bitfields anymore: the old
   rules "no bit-int variables" / "no bit-int params" are gone (a container is
   legal in every position). The mask-invariant verifier below takes their place.

   The one structural rule that remains:
     records/unions/tuples must have a byte-multiple total size (no 7-bit, 19-bit,
     ... aggregates), so the builtin parser/deparser can operate on a byte stream
     rather than a bit stream. *)

let error msg = Console.show_message msg ANSITerminal.Red "error"; raise (TypeError "")
;;
let err_in_decl decl_opt str = 
  match decl_opt with
  | None -> 
    error str
  | Some decl -> error @@ 
      str^"\n"
      ^(Printf.sprintf 
        "\n------ in declaration ----- \n%s\n----------------------\n" 
        (CCorePPrint.decl_to_string decl))

(* (the old "rule 3" record/tuple byte-multiple checker was removed: it used the
   *container* size (bitsizeof: bool = 8, sub-byte ints rounded), which contradicts the
   bit-packed wire model. The wire byte-multiple constraint is now enforced per event by
   CCoreUtils.check_r1_widths (logical sizeof: bool = 1) in the feature gate, and only
   events are serialized, so in-memory records need no byte-multiple constraint.) *)

(* Deferred / still-unsupported ops that have no C lowering yet and print as
   pseudo-operators (so they emit invalid C if used; same as before -- a program
   using them won't gcc-compile):
     - Conc (concatenation, Lucid `^`) -> "++"
     - SatPlus/SatSub (`|+|`/`|-|`)    -- need a real saturating lowering (clamp at
       the type max 2^n-1).
   (Slice IS now lowered -- CCoreMaskWidths rewrites it to a shift+mask.)
   These are left as a note rather than a hard error for now, to avoid flagging
   the operators.dpt regression test (which exercises them) as a build failure. *)

(* the mask invariant (replaces the old "no bit-int variables / params" rules):
   every width-violating producer with a non-standard, maskable result must be
   masked -- i.e. it must be the first operand of a BitAnd whose second operand is
   a constant (exactly the shape CCoreMaskWidths produces). A missed mask is a
   silent wrong-value bug, so we turn it into a compile error here. (Read/Peek are
   already lowered to helper calls by this point, so the surviving violating ops
   are arithmetic / Cast / Hash.) *)
let is_maskable_result ty =
  match ty.raw_ty with TInt n -> CCoreMaskWidths.maskable n | _ -> false

let mask_invariant_checker = object
  inherit [_] s_iter as super
  val mutable cur_decl_opt = None
  val mutable masked = []   (* physical identities of masked sub-expressions *)
  method! visit_decl () decl =
    cur_decl_opt <- Some decl;
    super#visit_decl () decl
  method! visit_exp () exp =
    (match exp.e with
     | EOp(BitAnd, [inner; { e = EVal _; _ }]) -> masked <- inner :: masked
     | EOp(op, _)
       when CCoreMaskWidths.is_violating_op op
            && is_maskable_result exp.ety
            && not (List.memq exp masked) ->
       err_in_decl cur_decl_opt @@ Printf.sprintf
         "non-standard-width value is not masked (CCoreMaskWidths should have wrapped it): >>> %s <<<"
         (CCorePPrint.exp_to_string exp)
     | _ -> ());
    super#visit_exp () exp
end
;;


(* make sure that all OTupleLocal and OTupleAssign ops have been eliminated *)
(* the only time these constructs appear is in Table.lookup return values, 
   when the frontend eliminates records and user-defined tuples. *)
let tuple_assign_checker = object 
  inherit [_] s_iter as super 
  val mutable cur_decl_opt = None
  method! visit_decl () decl = 
    cur_decl_opt <- Some decl;
    super#visit_decl () decl

  method! visit_OTupleLocal _ _ _ = 
    err_in_decl cur_decl_opt "tuple locals are not supported in C"
  method! visit_OTupleAssign _ _ = 
    err_in_decl cur_decl_opt "tuple locals are not supported in C"
end

(* make sure there are no TBit-typed values. 
   These should be removed from lucid entirely. 
   They were just added for ternary tables, but we can just as well 
   do it with a (value, mask) pair. *)
let tbit_checker = object 
  inherit [_] s_iter as super 
  val mutable cur_decl_opt = None
  method! visit_decl () decl = 
    cur_decl_opt <- Some decl;
    super#visit_decl () decl

  method! visit_ty () ty = 
    super#visit_ty () ty;
    match ty.raw_ty with 
    | TBits _ -> 
      err_in_decl cur_decl_opt "TBit types are not supported in C"
    | _ -> ()
end




(* ===== early feature gate (run right after CoreToCCore, before any lowering) =====
   Reject programs that use C-backend features not yet supported, with clear errors
   in terms of the source program (before lowering mangles things):
     1. event fields whose leaves are not byte-multiple widths. The codec byte-pads
        each field (writes/reads ceil(n/8) bytes), which only matches Lucid's
        bit-packed wire semantics (Tofino, interpreter) when every field is
        byte-aligned. Sub-byte fields need the not-yet-implemented bit-packing layer.
        (Sub-byte *computation* inside a program is fine -- see CCoreMaskWidths; this
        only restricts what crosses the wire.)
     2. concatenation (Conc, Lucid `^`) -- no C lowering yet.
     3. saturating arithmetic (SatPlus/SatSub, `|+|`/`|-|`) -- needs a real lowering
        that clamps at the type max 2^n-1.
   (This is separate from all_checks, which runs late -- the mask-invariant verifier
   there needs CCoreMaskWidths to have run first.) *)

(* an event field must be a scalar the codec can (de)serialize: an int (any width)
   or a bool. Aggregate fields (struct/tuple/vec) are not supported yet -- serializing
   one correctly would need the C struct packed to the wire layout. The midend only
   flattens records-with-globals and tuples, so a plain user struct (or one also used in
   a table) survives into an event as a nested record.
   Sub-byte / non-byte-multiple int fields ARE now allowed, provided the event's fields
   pack into a byte-multiple bitstream under restriction R1' (see ccore-refactor-notes
   §21 and CCoreUtils.check_r1_widths): a field may cross a byte boundary only if it is
   byte-aligned at its start or its end. The bit-packed codec relies on this. *)
let check_event_fields decls =
  match CCoreVariants.find_variant_sigs decls with
  | None -> ()
  | Some sigs ->
    List.iter
      (fun s ->
        let ed = CCoreVariants.sig_to_event_def s in
        let ename = CCorePPrint.cid_to_string ed.evconstrid in
        (* 1. every field must be a scalar int/bool *)
        List.iter
          (fun (fid, fty) ->
            match (base_type fty).raw_ty with
            | TInt _ | TBool -> ()
            | _ ->
              error @@ Printf.sprintf
                "event %s: field %s has an aggregate type (%s). Struct/tuple/vector event \
                 fields are not supported by the C backend yet (the packet (de)serializer \
                 only handles scalar int/bool fields) -- flatten the header into scalar \
                 fields for now."
                ename (CCorePPrint.cid_to_string fid) (CCorePPrint.ty_to_string fty))
          ed.evparams;
        (* 2. the field bitstream must satisfy R1' and be byte-multiple. The widths are
              the on-wire LOGICAL widths (sizeof_ty: int<n> = n, bool = 1 -- matching the
              bit-packed codec and the interpreter). Fields start byte-aligned in both
              packet and background events (the eth/tag framing is byte-multiple). *)
        let widths = List.map (fun (_, fty) -> sizeof_ty fty) ed.evparams in
        match CCoreUtils.check_r1_widths widths with
        | Ok () -> ()
        | Error (i, reason) ->
          let where =
            if i < List.length ed.evparams
            then Printf.sprintf "field %s" (CCorePPrint.cid_to_string (fst (List.nth ed.evparams i)))
            else "its fields"
          in
          error @@ Printf.sprintf "event %s: %s -- %s" ename where reason)
      sigs

let unsupported_op_gate = object
  inherit [_] s_iter as super
  val mutable cur_decl_opt = None
  method! visit_decl () decl = cur_decl_opt <- Some decl; super#visit_decl () decl
  method! visit_exp () exp =
    super#visit_exp () exp;
    match exp.e with
    | EOp(Conc, _) ->
      err_in_decl cur_decl_opt "concatenation (Lucid `^`) is not yet supported in the C backend"
    | EOp((SatPlus | SatSub), _) ->
      err_in_decl cur_decl_opt "saturating arithmetic (`|+|` / `|-|`) is not yet supported in the C backend"
    (* generate_ports (multicast) becomes an egen_group call in CoreToCCore. The C
       backend's out_event model sends each generated event to a single port, so it can't
       fan out to a group -- reject it here rather than silently mis-compiling it as a
       single-port send (the group id used as a port). *)
    | _ when is_egen_group exp ->
      err_in_decl cur_decl_opt "generate_ports (multicast) is not supported by the C backend \
                                -- each generated event is sent to a single port; flooding a \
                                port group is not implemented"
    | _ -> ()
end

let feature_gate decls =
  check_event_fields decls;
  List.iter (fun d -> unsupported_op_gate#visit_decl () d) decls

let all_checks decls =
  List.iter (fun decl ->
    mask_invariant_checker#visit_decl () decl;
    tuple_assign_checker#visit_decl () decl;
    tbit_checker#visit_decl () decl;

  )
  decls
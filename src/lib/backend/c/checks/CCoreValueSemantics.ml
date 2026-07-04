(* Read-only diagnostic for the (planned) value-semantics boundary.

   At the point in the pipeline where Lucid's abstractions have been
   lowered into ordinary IR (events, handlers, tables, parsers, memops)
   but before the C-specific passes run, the IR is *intended* to be
   value-semantic: no raw pointers, no address-of, no dereference.

   Arrays are NOT a violation: they are value aggregates, currently
   spelled TPtr(_, Some _) (the future TVec). Only bare pointers
   (TPtr(_, None)), EAddr, and EDeref count as leaks across the boundary.

   This pass changes nothing. It walks the IR and reports every leak,
   grouped by kind and by the declaration it appears in, so we can see
   the real pointer surface before committing to the TPtr/TVec split and
   a hard check. *)

open CCoreSyntax

let decl_name d =
  match d.d with
  | DFun (_, cid, _, _, _) -> CCorePPrint.cid_to_string cid
  | DVar (cid, _, _) -> CCorePPrint.cid_to_string cid
  | DTy (cid, _) -> CCorePPrint.cid_to_string cid
  | DForiegn _ -> "<foreign>"
;;

(* the categories we report, in display order *)
let categories = [ "EDeref"; "EAddr"; "TPtr(None)" ]

let collector =
  object (self)
    inherit [_] s_iter as super

    val mutable cur_decl = "<top-level>"
    (* count of array-shaped pointer *types* (fine: these become TVec) *)
    val mutable arrays = 0
    (* count of value-semantic array *accesses*: EDeref(arr + idx) where
       arr has array type. These read as `arr[i]` and are NOT leaks. *)
    val mutable array_idx = 0
    (* genuine leaks: (category, enclosing-decl) *)
    val mutable leaks : (string * string) list = []

    method reset () =
      cur_decl <- "<top-level>";
      arrays <- 0;
      array_idx <- 0;
      leaks <- []

    method arrays = arrays
    method array_idx = array_idx
    method leaks = leaks
    method private leak cat = leaks <- (cat, cur_decl) :: leaks

    method! visit_decl env decl =
      match decl.d with
      (* foreign C (the raw-target escape hatch) is not value-semantic IR, so a
         pointer in its signature/body is expected C plumbing -- exempt it. *)
      | DForiegn _ -> ()
      | DFun(_, _, _, _, BForiegn _) -> ()
      | _ ->
        cur_decl <- decl_name decl;
        super#visit_decl env decl

    method! visit_TPtr env t alo =
      super#visit_TPtr env t alo;
      (match alo with
      | Some _ -> arrays <- arrays + 1 (* array aggregate (legacy ptr spelling): OK *)
      | None -> self#leak "TPtr(None)") (* bare pointer: leak *)

    method! visit_TVec env t l =
      super#visit_TVec env t l;
      arrays <- arrays + 1 (* value-semantic vector type: OK above the boundary *)

    method! visit_EAddr env cid =
      super#visit_EAddr env cid;
      self#leak "EAddr"

    method! visit_EDeref env e =
      super#visit_EDeref env e;
      (* indexing is now EOp(Idx,..); any remaining deref is a genuine pointer *)
      self#leak "EDeref"

    method! visit_EOp env op args =
      super#visit_EOp env op args;
      (match op with
       | Idx -> array_idx <- array_idx + 1 (* value-semantic vector index: OK *)
       | _ -> ())
  end
;;

(* run the diagnostic and print a grouped summary. read-only. *)
let report label decls =
  collector#reset ();
  List.iter (collector#visit_decl ()) decls;
  let leaks = collector#leaks in
  let arrays = collector#arrays in
  Printf.printf "==== value-semantics diagnostic: %s ====\n" label;
  Printf.printf "  array types (TPtr Some -> TVec, OK)   : %d\n" arrays;
  Printf.printf "  array accesses (arr[i], value-sem, OK): %d\n" collector#array_idx;
  Printf.printf "  genuine pointer leaks above boundary  : %d\n" (List.length leaks);
  List.iter
    (fun cat ->
      let in_cat = List.filter (fun (c, _) -> c = cat) leaks in
      if in_cat <> []
      then begin
        let decls_for = List.map snd in_cat in
        let uniq = List.sort_uniq compare decls_for in
        let decl_summary =
          String.concat
            ", "
            (List.map
               (fun d ->
                 let n = List.length (List.filter (( = ) d) decls_for) in
                 if n > 1 then Printf.sprintf "%s(x%d)" d n else d)
               uniq)
        in
        Printf.printf "    %-11s x%-3d in: %s\n" cat (List.length in_cat) decl_summary
      end)
    categories;
  Printf.printf "=============================================\n"
;;

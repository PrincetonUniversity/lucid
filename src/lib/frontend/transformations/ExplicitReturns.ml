open Batteries
open Syntax
open Collections

(*** Rewrites the program to make the control flow of returns explicit; i.e. to
     ensure no code ever appears after a return statement on any control path.
     This is necessary to make inlining work, since inlining turns returns into
     regular assignment statements, which don't terminate computation. ***)

type env = Id.t IdMap.t

(* Assign the variable retvar to true before each return statement *)
let insert_retvar retvar s =
  let v =
    object
      inherit [_] s_map

      (* Save a little work *)
      method! visit_exp _ e = e

      method! visit_SRet _ e =
        let asn = sassign (LId retvar) (value_to_exp (vbool true)) in
        SSeq (asn, sret_sp e Span.default)
    end
  in
  v#visit_statement () s
;;

let adjust_returns prog =
  (* Track whether or not we've returned.
     Some true = have returned, Some false = haven't returned,
     None = conditionally returned (i.e. returned along some paths but not all of them) *)
  (* Note that returned is strictly an output of the visit functions; they never
     read from it without first setting it (via a recursive call). Unfortunately,
     we have to get that output via ref because of the way the s_map class is
     set up. *)
  let returned = ref None in
  let merge r1 r2 = if r1 = r2 then r1 else None in
  let retvar = ref None in
  let set_retvar () =
    match !retvar with
    | Some _ -> ()
    | None -> retvar := Some (Id.fresh "returned")
  in
  (* Add assignments to retvar before every return statement in s1, and wrap
     s2 in an if statement so it's only executed if we haven't returned yet. *)
  let add_retvar_if s1 s2 =
    set_retvar ();
    let not_returned_exp =
      op_sp Not [var_sp (Id (Option.get !retvar)) Span.default] Span.default
    in
    SSeq (insert_retvar (Option.get !retvar) s1, sifte not_returned_exp s2 snoop)
  in
  let v =
    object (self)
      inherit [_] s_map

      (* Save a little work -- we're only processing at the statment level *)
      method! visit_exp _ e = e

      method! visit_statement _ s =
        (* We always assume we haven't returned yet when starting to check a
           statement, since otherwise we wouldn't run it *)
        returned := Some false;
        { s with s = self#visit_s () s.s }

      method! visit_SRet _ e =
        returned := Some true;
        SRet e

      method check_SIf left right =
        let left = self#visit_statement () left in
        let left_returned = !returned in
        let right = self#visit_statement () right in
        let right_returned = !returned in
        left, right, left_returned, right_returned

      (* Same as check_SIf, but for lists instead of pairs *)
      method check_SMatch branches =
        List.split
        @@ List.map
             (fun (p, s) ->
               let s = self#visit_statement () s in
               (p, s), !returned)
             branches

      method! visit_SIf _ e left right =
        let left, right, lr, rr = self#check_SIf left right in
        returned := merge lr rr;
        SIf (e, left, right)

      method! visit_SMatch _ es branches =
        let branches, rets = self#check_SMatch branches in
        returned := List.fold_left merge (List.hd rets) rets;
        SMatch (es, branches)

      method! visit_SSeq _ s1 s2 =
        let s1 = self#visit_statement () s1 in
        (* If s1 always returns, there'll be no point in visiting s2, so
           delay that visit until it's necessary. *)
        let s2' () = self#visit_statement () s2 in
        (* Check if s1 always/never/sometimes returns *)
        match !returned with
        | Some false -> SSeq (s1, s2' ())
        | Some true ->
          Console.warning_position
            s2.sspan
            "This code appears after a return statement";
          (* Just return s1 to eliminate the dead code *)
          s1.s
        | None ->
          (match s1.s with
          | SIf (e, left, right) ->
            (* We're duplicating some work here since we already went through
               the body of this if on the recursive call, but oh well *)
            let _, _, left_returned, right_returned =
              self#check_SIf left right
            in
            (* TODO: Make sure this returns the right returned := thing *)
            begin
              match left_returned, right_returned with
              (* Impossible -- would have been caught by the first match *)
              | Some true, Some true | Some false, Some false ->
                failwith "impossible"
              (* If exactly one branch returned, we can safely move the
                 entirety of s2 inside the other branch *)
              | Some true, _ -> SIf (e, left, sseq right (s2' ()))
              | _, Some true -> SIf (e, sseq left (s2' ()), right)
              (* If either branch conditionally returned, and neither always
                 returns, we need to add a variable to track if we've returned *)
              | None, _ | _, None -> add_retvar_if s1 (s2' ())
            end
          | SMatch (es, branches) ->
            let _, rets = self#check_SMatch branches in
            (* If exactly one branch doesn't always return... *)
            if List.count_matching (fun x -> x <> Some true) rets = 1
            then (
              (* ...we can move s2 inside the branch without code duplication *)
              let idx =
                (* Index of the branch that doesn't always return *)
                match List.index_of (Some false) rets with
                | Some idx -> idx
                | None -> Option.get (List.index_of None rets)
              in
              let branches =
                List.modify_at idx (fun (p, s) -> p, sseq s (s2' ())) branches
              in
              SMatch (es, branches))
            else
              (* Otherwise, we need to use the same trick with the retvar *)
              add_retvar_if s1 (s2' ())
          | SLoop _ ->
            (* If we conditionally returned inside a loop, we always have to use
               the retvar trick *)
            add_retvar_if s1 (s2' ())
          | _ ->
            failwith
              "Somehow got unknown return status after a statement that wasn't \
               an If, Match or Loop")

      method! visit_decl _ d =
        match d.d with
        | DFun (id, ret_ty, specs, body) ->
          returned := Some false;
          let body = self#visit_body () body in
          (* Make sure that all paths lead to a return statement *)
          if !returned <> Some true && ret_ty.raw_ty <> TVoid
          then
            Console.error_position d.dspan "Non-void function may not return!";
          let body =
            match !retvar with
            | None -> body
            | Some id ->
              let params, s = body in
              let dec = slocal id (ty TBool) (value_to_exp (vbool false)) in
              params, sseq dec s
          in
          { d with d = DFun (id, ret_ty, specs, body) }
        | _ -> d
    end
  in
  v#visit_decls () prog
;;

open Batteries
open Syntax

(*** Check the program to ensure that handlers for non-control entry events
     never generate a non-exit event. Since events are regular values,
     this involves a dataflow analysis. For simplicity, we assume that inlining
     has been performed already, but it shouldn't be too hard to extend. ***)

module VarSet = Set.Make (Id)

type env =
  { linerate_entrys : VarSet.t
  ; exit_defs : VarSet.t
  ; exit_vars : VarSet.t
  ; in_if : bool
  }

let rec is_exit env e =
  match e.e with
  | EVar (Id id) | EVal { v = VEvent { eid = Id id; _ } } ->
    VarSet.mem id env.exit_vars
  | ECall (cid, args) ->
    (match cid with
    | Compound (id, _) when Id.equal id (Id.create "Event") ->
      (* Uses the property that each Event.xxx function takes exactly one event
         argument. If this changes, we'll probably need to know which argument
         position to check for each Event function. *)
      List.exists (is_exit env) args
    | Id id -> VarSet.mem id env.exit_defs
    | _ -> false)
  | EInt _ | EOp _ | EHash _ -> false
  | _ -> false
;;

(* Unfortunately, scope isn't baked into the structure of our syntax the way it
   is in OCaml. This means that we need to maintain a global environment instead
   of threading it through function calls. This also means that we need to reset
   that environment at the end of each scope. A scope is created every time we
   recurse into a statement, except in SSeq. *)
let check prog =
  let v =
    object (self)
      inherit [_] s_iter

      val mutable env : env =
        { linerate_entrys = VarSet.empty
        ; exit_defs = VarSet.empty
        ; exit_vars = VarSet.empty
        ; in_if = false
        }

      method! visit_d dummy d =
        (* print_endline @@ "Working on:" ^ Printing.d_to_string d; *)
        match d with
        | DEvent (x, s, _, _) ->
          (match s with
          | EEntry false ->
            env
              <- { env with linerate_entrys = VarSet.add x env.linerate_entrys }
          | EExit -> env <- { env with exit_defs = VarSet.add x env.exit_defs }
          | _ -> ())
        | DHandler (x, body) ->
          if VarSet.mem x env.linerate_entrys
          then (
            let orig_env = env in
            self#visit_body dummy body;
            env <- orig_env)
        | ConstVar (x, { raw_ty = TEvent _; _ }, exp) ->
          if is_exit env exp
          then env <- { env with exit_vars = VarSet.add x env.exit_vars }
          else ()
        | _ -> ()

      method! visit_SLocal _ x _ body =
        if is_exit env body
        then env <- { env with exit_vars = VarSet.add x env.exit_vars }
        else env <- { env with exit_vars = VarSet.remove x env.exit_vars }

      method! visit_SAssign _ x body =
        if is_exit env body
        then env <- { env with exit_vars = VarSet.add x env.exit_vars }
        else env <- { env with exit_vars = VarSet.remove x env.exit_vars }

      method! visit_SIf dummy _ left right =
        let orig_env = env in
        env <- { orig_env with in_if = true };
        self#visit_statement dummy left;
        let left_exit_vars = env.exit_vars in
        env <- { orig_env with in_if = true };
        self#visit_statement dummy right;
        env
          <- { orig_env with
               exit_vars = VarSet.inter left_exit_vars env.exit_vars
             }

      method! visit_SMatch dummy _ branches =
        let orig_env = env in
        match branches with
        | [] -> Console.error "Match with no branches?"
        | (_, s) :: tl ->
          env <- { orig_env with in_if = true };
          self#visit_statement dummy s;
          let exit_vars = env.exit_vars in
          let exit_vars =
            List.fold_left
              (fun acc (_, s) ->
                env <- { orig_env with in_if = true };
                self#visit_statement dummy s;
                VarSet.inter acc env.exit_vars)
              exit_vars
              tl
          in
          env <- { orig_env with exit_vars }

      (* Note that visit_d doesn't go into the body except for linerate
         handlers, so it's always ok to raise a warning *)
      method! visit_SGen _ _ e =
        if is_exit env e
        then ()
        else if env.in_if
        then
          Console.warning_position
            e.espan
            "Conditional generation of potential non-exit event in entry \
             handler."
        else
          Console.warning_position
            e.espan
            "Unconditional generation of potential non-exit event in entry \
             handler!"
    end
  in
  v#visit_decls () prog
;;

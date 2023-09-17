open Batteries
open CoreSyntax
open InterpCore
open InterpState
open Collections

type t =
  { events : (event_sort * int option * ty list) Env.t
  ; externs : ty Env.t
  ; extern_funs : IdSet.t
  }

let empty =
  { events = Env.empty
  ; externs =
      Env.singleton
        (Id Builtins.recirc_id)
        (SyntaxToCore.translate_ty Builtins.recirc_ty)
  ; extern_funs = IdSet.empty
  }
;;

(* assign a number to each event *)
let set_event_nums decls =
  let event_nums = List.filter_map 
    (fun decl -> match decl.d with
      | DEvent(_, nopt, _, _) -> nopt
      | _ -> None)
    decls
  in
  let rec set_event_nums' num decls = 
    if (List.exists (fun v -> v = num) event_nums)
    then set_event_nums' (num+1) decls
    else 
      match decls with
      | [] -> []
      | decl::decls -> (
        match decl.d with
        | DEvent(a, None, b, c) -> 
          {decl with d = DEvent(a, Some(num), b, c)}::(set_event_nums' (num+1) decls)
        | _ -> decl::(set_event_nums' num decls)
      )
  in
  set_event_nums' 1 decls
;;




(* Remove and process declarations which we can/must do beforehand --
   extern/event declarations, and precompute size values *)
let preprocess ds =
  let ds = set_event_nums ds in (* set event numbers *)
  let pp, ds =
    List.fold_left
      (fun (pp, ds) d ->
        match d.d with
        | DEvent (id, num, sort, params) ->
          (match num with 
          | None -> print_endline ("event " ^ (CorePrinting.id_to_string id) ^ " has no num")
          | _ -> ());
          ( { pp with
              events = Env.add (Id id) (sort, num, List.map snd params) pp.events
            }
          , d :: ds )
        | DExtern (id, ty) -> begin
          match ty.raw_ty with
          | TFun _ -> { pp with extern_funs = IdSet.add id pp.extern_funs }, ds
          | _ -> { pp with externs = Env.add (Id id) ty pp.externs }, ds
        end
        | _ -> pp, d :: ds)
      (empty, [])
      ds
  in
  pp, List.rev ds
;;

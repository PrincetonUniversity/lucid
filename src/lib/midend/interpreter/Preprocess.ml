open Batteries
open CoreSyntax
open InterpCore
open InterpState

type t =
  { events : (event_sort * ty list) Env.t
  ; externs : ty Env.t
  }

let empty =
  { events = Env.empty
  ; externs =
      Env.singleton
        (Id Builtins.recirc_id)
        (SyntaxToCore.translate_ty Builtins.recirc_ty)
  }
;;

(* Remove and process declarations which we can/must do beforehand --
   extern/event declarations, and precompute size values *)
let preprocess ds =
  let pp, ds =
    List.fold_left
      (fun (pp, ds) d ->
        match d.d with
        | DEvent (id, sort, params) ->
          ( { pp with
              events = Env.add (Id id) (sort, List.map snd params) pp.events
            }
          , d :: ds )
        | DExtern (id, ty) ->
          { pp with externs = Env.add (Id id) ty pp.externs }, ds
        | _ -> pp, d :: ds)
      (empty, [])
      ds
  in
  pp, List.rev ds
;;

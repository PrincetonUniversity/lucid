open Batteries
open CoreSyntax
open InterpCore
open InterpState
open Collections
open PreprocessHeaders

type t =
  { events : (id option * ty list) Env.t
  ; headers : pkt_entry IdMap.t * header_trie
  ; externs : ty Env.t
  ; extern_funs : IdSet.t
  }

let empty =
  { events = Env.empty
  ; headers = IdMap.empty, HTrie.empty
  ; externs =
      Env.singleton
        (Id Builtins.recirc_id)
        (SyntaxToCore.translate_ty Builtins.recirc_ty)
  ; extern_funs = IdSet.empty
  }
;;

(* Remove and process declarations which we can/must do beforehand --
   extern/event declarations, and precompute size values *)
let preprocess header_decs ds =
  let pp = { empty with headers = preprocess_headers header_decs } in
  let pp, ds =
    List.fold_left
      (fun (pp, ds) d ->
        match d.d with
        | DEvent (id, pkt_ty, params) ->
          ( { pp with
              events = Env.add (Id id) (pkt_ty, List.map snd params) pp.events
            }
          , d :: ds )
        | DExtern (id, ty) ->
          begin
            match ty.raw_ty with
            | TFun _ ->
              { pp with extern_funs = IdSet.add id pp.extern_funs }, ds
            | _ -> { pp with externs = Env.add (Id id) ty pp.externs }, ds
          end
        | _ -> pp, d :: ds)
      (pp, [])
      ds
  in
  pp, List.rev ds
;;

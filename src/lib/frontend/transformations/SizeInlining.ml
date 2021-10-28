open Batteries
open Syntax
open SyntaxUtils
open Collections

(* Inline all user sizes. Should remove all DSize declarations. Expects
   modules to be eliminated first. *)

let replacer =
  object (self)
    inherit [_] s_map

    method! visit_DSize env id sz =
      let sz = Option.get sz in
      let sz = self#visit_size env sz in
      env := CidMap.add (Id id) sz !env;
      (* We will filter this declaration later *)
      DSize (id, Some sz)

    method! visit_IUser env cid =
      match CidMap.find_opt cid !env with
      | Some sz -> sz
      | None -> IUser cid
  end
;;

let replace_prog ds =
  let ds = replacer#visit_decls (ref CidMap.empty) ds in
  List.filter
    (function
      | { d = DSize _ } -> false
      | _ -> true)
    ds
;;

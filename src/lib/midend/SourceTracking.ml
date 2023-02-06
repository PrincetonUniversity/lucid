(* operations on metadata of syntax nodes
 (e.g., to track node provenance throughout compilation) *)
open Collections
open Syntax
open SyntaxUtils

type refmap_t = (Cid.t list) CidMap.t
type refs = {
  (* a map from globals to the globals that they reference *)
  refmap : refmap_t;
  (* globals to the globals they are ref'd by *)
  refmap_rev : refmap_t;  
}
let empty_refs =  {refmap = CidMap.empty; refmap_rev = CidMap.empty;}

let refmap_append refmap k v =
  match (CidMap.find_opt k refmap) with
    | Some(vs) -> CidMap.add k (vs@[v]) (CidMap.remove k refmap) 
    | _ -> CidMap.add k [v] refmap
;;

let add_refs refs usercid usedcids =
  List.fold_left
    (fun refs usedcid -> 
      { refmap = refmap_append refs.refmap usercid usedcid;
        refmap_rev = refmap_append refs.refmap_rev usedcid usercid;})
    refs
    usedcids
;;

let refmap_to_string refmap =
  CidMap.fold
    (fun k vs strs -> 
      strs@[(Printing.cid_to_string k)^" : ["^(Printing.comma_sep Printing.cid_to_string vs)^"]"])
    refmap
    []
  |> String.concat "\n"
;;

type ctx = {
  module_path : id list;
}
let empty_ctx = {module_path = [];}

let enter_module ctx id =
  {module_path = ctx.module_path@[id];}
;;

(* initialize the tracking metadata 
   setting all the source identifiers 
   to current ids *)
let init_tracking ds = 
  let refs = ref empty_refs in
  let ref_collector =
    object (self)
      inherit [_] s_iter as super
      method! visit_decl ctx decl = 
        match decl.d with
        | DModule(id, intf, decls) ->
          let ctx' = enter_module ctx id in
          self#visit_interface ctx' intf;
          self#visit_decls ctx' decls
        (* globals are the things we really want to track... *)
        | DGlobal(id, _, exp) -> (
          match exp.e with
            | ETableCreate({tactions=tactions;}) ->
              (* update reference map *)
              refs := add_refs 
                (!refs) 
                (Cid.id id) 
                (List.map cid_of_exp tactions);
            (* not a table, just recurse *)
            | _ -> super#visit_decl ctx decl
        )
        | _ -> 
          super#visit_decl ctx decl
      end
  in
  ref_collector#visit_decls empty_ctx ds;
  let refs = !refs in

  print_endline "----- SourceTracking derived refmap ------";
  print_endline (refmap_to_string (refs.refmap));
  print_endline "----- SourceTracking derived reverse refmap ------";
  print_endline (refmap_to_string (refs.refmap_rev));
  print_endline "----- END SourceTracking derived reverse refmap ------";
  ds
;;

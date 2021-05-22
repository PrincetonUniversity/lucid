(* Misc builtins. Mainly Sys.time() *) 
open Syntax
open InstrSyntax
open InterpHelpers
open TofinoContext
open TofinoOp
open TofinoConstants

(* "interface" for this module -- the functions implemented here. *)
let module_name = "Sys"
let module_id = Id.create module_name
let cid_of_fname name_str = Cid.create_ids [module_id; Id.create name_str]
;;
let time_cid = cid_of_fname "time"
;;

let get_time (args : codegenInput) : codegenOutput = 
  match args.basename with 
  | Some basename -> (
    match args.retname with 
    | Some retname -> 
        let ivec_decl = IS.new_dsingleinstr (basename) retname (new_expr_of_mid (timestamp_name)) in 
        {names = [basename]; objs = [ivec_decl]}
    | _ -> error "[get_time] error: Sys.time() generator not provided with a return variable."
  )
  | None -> error "[get_time] opstmt doesn't have an id..."
;;

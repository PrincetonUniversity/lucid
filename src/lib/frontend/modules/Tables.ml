(* Tables as a builtin module *)
open Batteries
open Syntax
open InterpState


(* 
Taking a step back: 

  - I don't think tables actually need type arguments after all. 
  - The arguments to the constructor can just be unbound types. 
      - a list or tuple of actions
      - a default action
  - We need to specify the key size somewhere, and that can be a 
    tuple of sizes.
  - Todo: make sure that approach works out alright for Table.match and Table.install
  - It does, kind of. If you make all the types of the table functions be "anything", then 
    its fine for the type checker, but may permit invalid table calls. 
    - I think we could check all the Table expressions and declarations in a separate pass, after 
      type checking...
  Table.match : Table.t<'k> -> 'k -> 'a -> 'r
  Table.install : Table.t<'k> -> pat<'k> -> ('a -> 'r) -> ()
*)

(*  some type constructor helpers *)
let effectless_fun_rawty arg_tys ret_ty = 
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  TFun
    { arg_tys
    ; ret_ty
    ; start_eff
    ; end_eff = start_eff
    ; constraints = ref []
    }
;;

let acn_rawty arg_tys ret_ty =
  TAction {
    aarg_tys = arg_tys;
    aret_tys = [ret_ty];
  }
;;

let tynum = ref (-1)
let fresh_rawty base_tyname = 
  incr tynum;
  let ty_id = Id.create (base_tyname ^ string_of_int !tynum) in
  (TQVar (QVar ty_id))
;;
let fresh_ty base_tyname =  
  ty_sp (fresh_rawty base_tyname) Span.default
;;
let fresh_size base_id = 
  incr tynum;
  let size_id = Id.create ( base_id ^ string_of_int !tynum) in
   (IVar (QVar size_id))
;;

(* Generic  defs *)
let name = "Table"
let id = Id.create name

let error fun_name msg =
  error (name ^ ": " ^ fun_name ^ ": " ^ msg)
;;
let module_id = id
let t_id = Cid.create_ids [id; Id.create "t"]
let sizes = 0
let ty_args = 3
let global = true

let create_id = Cid.create_ids [id; Id.create "create"]
let create_sig =
  (* let size = IVar (QVar (Id.fresh "a")) in *)
  let eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  let key_ty = fresh_rawty "table_key_ty" in
  let arg_ty = fresh_rawty "table_arg_ty" in
  let ret_ty = fresh_rawty "table_ret_ty" in
  (* let acn_ty = acn_rawty [ty arg_ty] (ty ret_ty) in *)
  let acn_ty = fresh_rawty "tbl_acn_ty" in
  (* let acn_ty = effectless_fun_rawty [ty arg_ty] (ty ret_ty) in *)
  (* the trick is how we are relating the argument types to the module type of the return *)
  (* the arguments are the key and the action *)
  (* TODO: bound_acns_ty should be a vector, but then type checking fails after vector 
     elimination. So we either need: 
        1) vector types in backend;
        2) a way for vector elimination to change the signatures of builtin 
           functions when it runs. *)
  let bound_acns_ty = fresh_rawty "bound_acns_ty" in
  let arg_tys = [
      ty @@ TInt (IVar (QVar (Id.fresh "sz"))); (* number of entries *)
      ty @@ bound_acns_ty;                      (* actions bound to the table *)
      ty @@ acn_ty                              (* default action *)
    ] 
  in
  (* the return is a module parameterized with the args and return of the action *)
  let unbound_module_ty = TName (t_id, [], true, [key_ty; arg_ty; ret_ty]) in
  let ctor_ty = {
    arg_tys = arg_tys
    ; ret_ty = ty_eff unbound_module_ty eff
    ; start_eff
    ; end_eff = start_eff
    ; constraints = ref []
  } in
  ctor_ty 
;;

(* Table.add *)
let add_name = "add"
let add_id = Id.create add_name
let add_cid = Cid.create_ids [id; add_id]
let add_error msg = error add_name msg

let add_ty =
  let size = IVar (QVar (Id.fresh "a")) in
  let eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys =
           [ ty_eff (TName (t_id, [size], true, [])) eff
           ; ty @@ TInt size ]
       ; ret_ty = ty @@ TInt size
       ; start_eff
       ; end_eff = FSucc eff
       ; constraints = ref [CLeq (start_eff, eff)]
       }
;;

let dummy_memop = InterpSyntax.F (fun _ _ args -> extract_ival (List.hd args))
let setop = InterpSyntax.F (fun _ _ args -> extract_ival (List.nth args 1))
let dummy_int = InterpSyntax.V (CoreSyntax.vinteger (Integer.of_int 0))

let add_fun nst swid args =
  let open State in
  let open InterpSyntax in
  let open CoreSyntax in
  match args with
  | [V { v = VGlobal stage }; V { v = VInt addval }] ->
    let get_f arg = vinteger arg in
    let set_f arg = Integer.add arg addval in
    Pipeline.update ~stage ~idx:0 ~getop:get_f ~setop:set_f (sw nst swid).pipeline
  | _ ->
    add_error "Incorrect number or type of arguments to Counter.add"
;;

let constructors = [create_id, create_sig]

let defs : State.global_fun list =
  [{ cid = add_cid; body = add_fun; ty = add_ty }]
;;

let signature =
  (* let sz = IVar (QVar (Id.fresh "sz")) in *)
  let key_ty = fresh_rawty "table_key_ty" in
  let arg_ty = fresh_rawty "table_arg_ty" in
  let ret_ty = fresh_rawty "table_ret_ty" in
  let unbound_module_ty = TName (t_id, [], true, [key_ty; arg_ty; ret_ty]) in
  ( module_id
  , [Cid.last_id t_id, [], unbound_module_ty |> ty]
  , defs
  , constructors )
;;

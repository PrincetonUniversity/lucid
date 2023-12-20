(* Tables as a builtin module *)
open Batteries
open Syntax
open InterpState

(*  some helpers for readability *)
let effectless_fun_ty arg_tys ret_ty =
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  ty
  @@ TFun
       { arg_tys
       ; ret_ty
       ; start_eff
       ; end_eff = start_eff
       ; constraints = ref []
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

(* Constructor *)
(* 
// example usage:
// (16, 16) is the size of the table key, (32, 32) is the size of the table's actions
// 32 is the table's actions' args, 8 is the table's actions' return

global Table.t<(16, 16), 32, 8> mytbl = Table.create(1024, mk_default(1));
lhs: fully types
rhs: tint, tfun 

arg size = (32, 32)
ret size = 8

action int doit(int x, int y) {return 1;}

Typing the declaration: 
global Table.t<(16, 16), 32, 8> mytbl = Table.create(1024, doit);

Table.create(1024, mk_default(1));

------------------

one option is to make actions functions from ints to ints. 
and those ints can be sized with tuples.
oh that's so gross.

Table.create : int -> (int<'x> -> int<'y>) -> Table.t<'sk, 'x, 'y>
doit : int<(32, 32)> -> int<32>

action int doit(int<(32, 32)> (x, y))

----------------

without knowing anything about the call, write down the type of table create. 

(tup<'a> -> tup<'r>) -> Table.t<'sk, 'a, 'r>

and then when they come for the tuples... 


-----------------

What if the table type carried type arguments instead of size args? 
  Would that help? 

Table.create : int -> ('x -> 'y) -> Table.t<<int<32>, 'x, 'y>>

action int doit(int x, int y) {return 1;}
doit : (int, int) -> int

Table.create(1024, doit) : Table.t<'sk, 'x, 'y>

Yeah. That'd work. 

But that's a possibly hard modification...



*)

let create_id = Cid.create_ids [id; Id.create "create"]
let create_sig =
  let size = IVar (QVar (Id.fresh "a")) in
  let eff = FVar (QVar (Id.fresh "eff")) in
  let start_eff = FVar (QVar (Id.fresh "eff")) in
  let key_ty = fresh_rawty "table_key_ty" in
  let arg_ty = fresh_rawty "table_arg_ty" in
  let ret_ty = fresh_rawty "table_ret_ty" in
  let unbound_module_ty = TName (t_id, [], true, [key_ty; arg_ty; ret_ty]) in
  { arg_tys = [ty @@ TInt size; ty @@ key_ty]
  ; ret_ty = ty_eff unbound_module_ty eff
  ; start_eff
  ; end_eff = start_eff
  ; constraints = ref []
  }
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
  let key_ty = fresh_rawty "table_sig_key_ty" in
  let arg_ty = fresh_rawty "table_sig_arg_ty" in
  let ret_ty = fresh_rawty "table_sig_ret_ty" in
  let unbound_module_ty = TName (t_id, [], true, [key_ty; arg_ty; ret_ty]) in
  ( module_id
  , [Cid.last_id t_id, [], unbound_module_ty |> ty]
  , defs
  , constructors )
;;

(* translate each instance of 
   a builtin type into its appropriate backend 
   representation. 
   Also add function implementations for the 
   builtin functions that use values of those 
   types. *)
open FCoreSyntax
let err = Console.error ;;
let str = Printf.sprintf


(* left off here. *)
(* code generation for tables and actions *)
(* left off here. See table_repr.dpt for general strategy *)
module Table = struct 

  type tbl_code = 
    {
      tbl_closure_ty : ty;
      tbl_ty : ty;
      tbl_default_key : value; 
      tbl_default_action : value; (* This should be a closure value obtained by evaluating the call to the default action *)
      tbl_exp : exp;
      tbl_install_fun : decl;
      tbl_create_fun  : decl;
    }

  (* let rec process_decls tbl_defs (decls : decls) = 

  ;; *)

(* for each table: 
  1. derive the table type struct
    - table_context and table_type
  2. derive table_create, table_lookup, and table_install   
*)

(*** Table type generation ***)
(* We derive a table type from its constructor declaration 
    global Table.t<'k, 'a, 'r> tbl = Table.create(len, bound_action_constructors, constructor_call);


type tbl_closure = {
    Union({int a; int b} c1; {int a} c2) ctx; // the context has one record for each constructor in bound_action_constructors
    (int, int -> int) fcn; // the function's type is equal to the return type of all the bound_action_constructors
}

type tbl = 
    {
        'k key; // right from table param
        tbl_closure action; // generated for this table
    }[1024] entries;


// if we evaluated table_create, which we 
Tup [
  Tup [def_key, def_acn],
  ...    
  ]
]
where def_key is a wildcard and def_action is the action constructed by EVALUATING constructor_call


// table_install



*)




end

(* code generation for array. *)
module Array = struct 


end


let transform fdecls = 
  fdecls 
;;
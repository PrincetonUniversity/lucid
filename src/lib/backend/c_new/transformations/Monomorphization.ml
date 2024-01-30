(* translate each instance of 
   a builtin type into its appropriate backend 
   representation. 
   Also add function implementations for the 
   builtin functions that use values of those 
   types. *)
open FCoreSyntax
let err = Console.error ;;
let str = Printf.sprintf



(* code generation for tables and actions *)
(* left off here. See table_repr.dpt for general strategy *)
module Table = struct 
    (* Design: 
        A Table.t<k, a, r> is a primitive array where every slot holds: 
          a key of type pair<k>
          a closure of type a -> r

          
    
    
    *)


end

(* code generation for array. *)
module Array = struct 


end


let transform fdecls = 
  fdecls 
;;
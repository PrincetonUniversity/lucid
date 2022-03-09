(* make sure every variable's name is unique, except parameters *)
open CoreSyntax
open Printf
open Batteries
open InterpHelpers
module CL = Caml.List


(* map from names to ids *)
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

(* Make the string part of variable ids globally unique 
   This transformation may not be necessary anymore. *) 

let make_var_names_unique ds =
  (* Each time we reach a function-like thing, i.e., 
     something with parameters (an event or memop), 
     store the parameter ids in the context. 
     Each time we reach an id in the body of a function-like, 
     either claim ownership of the id's name for the id, 
     or if the name is claimed by another id, change the 
     id's name to id.name^str_of_int(id.num) *)
  let owner_of_name = ref StringMap.empty in
  let v =
    object (self) 
      inherit [_] s_map as super

      (* Save the parameters of the current event in the context. *)
      method! visit_body _ (params, statement) = 
        let new_stmt = self#visit_statement 
            (CL.split params |> fst) statement
        in 
        (params, new_stmt)

      (* Save the parameters of the current memop in the context. *)  
      method! visit_DMemop _ id params memop_body = 
        let param_ids = (CL.split params |> fst) in 
        let memop_body = self#visit_memop_body param_ids memop_body in 
        DMemop(id, params, memop_body)

      method! visit_params _ params = params     

      method! visit_id parameters id = 
        match (CL.exists (Id.equals id) parameters) with 
            (* skip parameters *)
            | true -> id
            | false -> (
                match (StringMap.find_opt (fst id) (!owner_of_name)) with 
                    (* not found --> claim name *)
                    | None -> 
                        owner_of_name := StringMap.add (fst id) id (!owner_of_name);
                        id
                    (* found --> if this is the owner, no change; else, bring number into name *)
                    | Some owner -> (
                        if (Id.equals owner id) 
                        then (id)
                        else (
                            let new_name = (fst id)^(string_of_int (snd id)) in 
                            (* print_endline ("[make_var_names_unique] changing name: "^(fst id)^" --> "^(new_name)); *)
                            new_name, (snd id)
                        )
                    )
            )
    end
  in
  v#visit_decls [] ds
;;

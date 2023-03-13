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
  (* to skip parameters, we store the parameter ids 
     of actions, memops, and handlers in context. *)
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
      method! visit_DMemop _ {mid=mid; mparams=mparams; mbody=mbody;} = 
        let mbody = self#visit_memop_body (CL.split mparams |> fst) mbody in 
        DMemop({mid; mparams; mbody})

      (* Save the parameters of the action in the context *)
      method! visit_DAction _ acn =
        super#visit_DAction
          (CL.split (acn.aconst_params@acn.aparams) |> fst)
          acn

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

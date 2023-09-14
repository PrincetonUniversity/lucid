(* Event combinators (just delay at this point) 
   aren't implemented yet -- just delete / warn for now *)

open CoreSyntax 

let warn str = 
  Console.show_message str ANSITerminal.Yellow "Unsupported feature"
;;


let rec process ds =   
  let v = 
    object
      inherit [_] s_map as super
      method! visit_exp ctx exp =
        let exp = super#visit_exp ctx exp in
        match exp.e with
        | ECall(fcn_cid, args) -> (
          match Cid.names fcn_cid with
          | "Event"::"delay"::_ -> (
            warn@@"removing event delay combinator in ("^(CorePrinting.exp_to_string exp)^" -- not yet supported";
            match args with 
            | ev_exp::_ -> ev_exp
            | _ -> exp
          )
          | _ -> exp
        )
        | _ -> exp
    end
  in
  v#visit_decls () ds
;;

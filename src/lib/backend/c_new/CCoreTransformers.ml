open CCoreSyntax

(* generic transformer functions *)

let subst_decl = object(_)
  inherit [_] s_map as super
  method! visit_decl transformer decl = 
    transformer decl
end

(* transform any statement *)
let subst_statement = object (_)
  inherit [_] s_map as super
  method! visit_statement transformer stmt = 
    (* apply the transformer *)
    let stmt = transformer stmt in
    (* descend *)
    super#visit_statement transformer stmt
  end
;;

(* transform any expression *)
let subst_exp = object (_)
  inherit [_] s_map as super
  method! visit_exp transformer exp = 
    let exp = transformer exp in
    super#visit_exp transformer exp
  end
;;

(* transform an evar *)
let subst_evar = object (_)
   inherit [_] s_map as super
   method! visit_exp transformer exp = 
    match exp.e with
      | EVar _ -> transformer exp
      | _ -> super#visit_exp transformer exp
   end
;;
(* transform a return statement  *)
let subst_return = object (_)
  inherit [_] s_map as super
  method! visit_statement transformer stmt = 
    match stmt.s with 
    | SRet(_) -> transformer stmt 
    | _ -> super#visit_statement transformer stmt
  end
;;
open CCoreSyntax
let replace n x xs = 
  let rec replace' n x xs acc = 
    match xs with 
    | [] -> List.rev acc
    | y::ys -> 
      if n = 0 then 
        List.rev_append (x::acc) ys
      else 
        replace' (n-1) x ys (y::acc)
  in
  replace' n x xs []
;;
let id = Id.create


let is_smatch statement = match statement.s with 
  | SMatch _ -> true 
  | _ -> false
;;

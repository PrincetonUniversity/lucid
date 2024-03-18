(* 
  simplify match statements for c. 

  TODO: after handler fusion.

  let to_bool_atom e p = 
    match p with 
      | PNum z -> Some(eop Eq [e; (eint z e.ety)])
      | PWild -> None
      | PBit ints -> 
        let v, m = ints_to_mask ints in
        Some(eop Eq [(e && m); (v && m)])

  then we do just do a fold over the branches
*)
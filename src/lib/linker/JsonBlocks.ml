(* 
  Generates P4 externs from json config file. 
  - entry event trigger table
  - exit event action table
*)

let generate configfn = 
  (JsonEntryTriggers.generate configfn)@
  (JsonExitActions.generate configfn)  
;;


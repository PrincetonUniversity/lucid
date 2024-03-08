(* implementation of Tables in CCore
   
  Implements: 

    1. Type TBuiltin(Cid["Table"; "t"], [key_type; const_arg_type; arg_type; result_type])

    2. Constructor Table.create(length, actions, default_action, default_action_const_arg);

    3. Method Table.lookup(Table_t, key, arg) -> result;

    4. Method Table.install(Table_t, key, action, const_arg);

*)
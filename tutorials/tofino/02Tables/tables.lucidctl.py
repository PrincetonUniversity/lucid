# this is an example lucid_ctl function that installs a rule.
def lucid_ctl(c, globals_dict):
  # get the P4 name of the "dmac_table"
  tbl_p4_name = globals_dict['wire_tbl']['compiled_name']
  # get the P4 name of the first action bound to the "dmac_table" (which is "out_port")
  action_p4_name = globals_dict['wire_tbl']["actions"][0]["compiled_name"]
  # define the key and action arguments. The key has one additional field in the P4 
  # program, a priority where lower is matched first. 
  key = [128, 1]
  # define the install-time action argument
  action_arg = [128]
  print(f"Installing rule in table {tbl_p4_name}: {key} -> {action_p4_name}({action_arg})")
  # call the table_install function to install the rule
  c.table_install(tbl_p4_name, key, action_p4_name, action_arg)

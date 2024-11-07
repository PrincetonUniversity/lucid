# a little debugging control loop. 
# add to the beginning of lucid.py control script in
# the build directory
def lucid_ctl(c, globals_dict):
  import time
  idx = 148
  while True:
    txct_val = c.array_get(globals_dict["txct"]["compiled_name"], idx)[0]
    rxct_val = c.array_get(globals_dict["rxct"]["compiled_name"], idx)[0]
    print(f"port {idx} debug -- txct: {txct_val} rxct: {rxct_val}")
    time.sleep(1)

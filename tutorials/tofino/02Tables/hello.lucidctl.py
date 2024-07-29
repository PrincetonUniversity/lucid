# this is an example lucid_ctl function that
# prints "hello world". 
# patch it into the compiler-generated 
# "lucid.py" file using the "patch_lucidpy.sh" script. 

def lucid_ctl(c, globals_dict):
  print("hello from custom lucid_ctl")
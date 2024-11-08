# Tables

This example is simple program that forwards packets according to a single table. The purpose of this program is to show how to write a python script that installs rules into a table in a Lucid program at runtime from the control plane. This example assumes that you already know how to compile and run a Lucid program on the tofino, i.e., you have already completed the `01Reflector` example.


## Building the example

Start by building the example as done in the previous example: `make build`

## globals.json

The lucid compiler generates a file, `globals.json`, in the build folder, that is a directory of the P4 names of all the actions and tables in the program. Here's the globals.json for this example:

```json
{
  "dmac_tbl": {
    "type": "table",
    "compiled_name": "pipe.ingress_hdl.dmac_tbl",
    "length": 1024,
    "keys": [
      {
        "compiled_name": "dmac_tbl_0_key",
        "size": 48,
        "matchtype": "ternary"
      },
      {
        "compiled_name": "$MATCH_PRIORITY",
        "size": 32,
        "matchtype": "exact"
      }
    ],
    "actions": [
      {
        "name": "out_port",
        "compiled_name": "ingress_hdl.dmac_tbl_out_port",
        "install_arg_sizes": [ 16 ]
      }
    ]
  }
}
```

If you have a preferred method of interacting with tables from the control plane, you can simply use the `globals.json` file to get the names of the tables and actions in the program, and then use those names with your preferred method. 

If you _don't_ have a preferred method, read on to see how to use lucid's control plane manager to interact with tables by extending the lucid-generated control plane script, `lucid.py`.

## Extending lucid.py with custom control code

`build/lucid.py` is the control plane script that the lucid compiler generates. We can extend this script to run a user-defined control function after the script installs all of Lucid's multicast rules. All we have to do is add a function with the signature: `lucid_ctl(ctl_mgr, globals_dict)` defined immediately before the `main` method in the script. After `lucid.py` installs all of Lucid's multicast rules, it will check for this function. If it exists, the script will call the function as the last step before exiting. The function can do whatever it wants and does not need to terminate because it runs after all of Lucid's internal setup is complete. 

The helper script `patch_lucidpy.sh <custom_code.py> <lucid.py>` inserts the contents of `<custom_code.py>` into `lucid.py` in the appropriate place, i.e., immediately before the `main` method.

Try it out: from this example's directory, run: 
```
make build
./patch_lucidpy.sh hello.lucidctl.py ./build/lucid.py
make assemble
```
then start the simulator with: 
```
make sim
```

You should see the following line towards the end of the output: 
```
[PY_MGR] hello from custom lucid_ctl
```

**Please note that the `lucid.py` script is generated by the lucid compiler and will be overwritten every time you run `make build`. If you want to keep your custom control code, you should save it in a separate file and use the `patch_lucidpy.sh` script to insert it into the `lucid.py` script as post-build processing.**

## Using Lucid's control plane manager to add table rules

The `lucid_ctl` function always takes two arguments:

1) a control plane manager object, `ctl_mgr` of class `Controller`, that you can use to interact with the tables and arrays in the program. the `Controller` class is defined in `build/libs/controldriver.py`, which is copied from `$LUCID_REPO/scripts/tofino/controldriver.py` by the lucid compiler.
2) a globals_dict object, which is a copy of the program-specific `globals.json` file that maps lucid object names to their P4 names.

Using these two objects, it is straightforward to interact with tables: simply find the names of the tables and the actions in the `globals_dict` object, and then use the `ctl_mgr` object to install rules into the tables.

An example for `tables.dpt` is in `tables.lucidctl.py`: 
```python
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
```

**note: to install a rule with a masked key, pass a tuple in for the field instead of a single value. For example, `key = [(128, 0xff), 1]` would match on only the first 8 bits of the first field.**

To run this control function, use the `patch_lucidpy.sh` script as before: 
```
./patch_lucidpy.sh tables.lucidctl.py ./build/lucid.py
```

After updating the `lucid.py` script with this control function, when you run the simulator, you should see the following log message indicating that the rule has been installed:
```
[PY_MGR] Installing rule in table pipe.ingress_hdl.wire_tbl: [128, 1] -> ingress_hdl.wire_tbl_out_port([128])
```

The rule reflects packets from port 128 back to port 128. 

To test that the rule is working, first run the simulator without installing the rule, i.e.: 
```
./patch_lucidpy.sh hello.lucidctl.py ./build/lucid.py
make sim
```
then in another terminal:
```
sudo ./send_pkt.py veth257
```
You should see the packet come out of port 129 in the simulator logs:
```
[SIM] :07-29 15:31:51.184611:    :-:0x1:<0,1,0>:========== Tx Pkt to port 129 (64 bytes) ==========
[SIM] :07-29 15:31:51.184625:    :-:0x1:<0,1,0>:Packet :
[SIM] :07-29 15:31:51.184641:        :-:0x1:<0,1,0>:04 8d 38 7f 66 b9 54 bf 64 93 17 a0 08 00 45 00
[SIM] :07-29 15:31:51.184656:        :-:0x1:<0,1,0>:00 14 00 01 00 00 40 00 6c a4 0a 00 00 40 01 02
[SIM] :07-29 15:31:51.184671:        :-:0x1:<0,1,0>:03 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 15:31:51.184685:        :-:0x1:<0,1,0>:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
```


Then do: 
```
./patch_lucidpy.sh tables.lucidctl.py ./build/lucid.py
make sim
```
and run the packet sender again. This time, the packet should come out of port 128:

```
[SIM] :07-29 15:43:36.115285:    :-:0x1:<0,1,0>:========== Tx Pkt to port 128 (64 bytes) ==========
[SIM] :07-29 15:43:36.115319:    :-:0x1:<0,1,0>:Packet :
[SIM] :07-29 15:43:36.115355:        :-:0x1:<0,1,0>:04 8d 38 7f 66 b9 54 bf 64 93 17 a0 08 00 45 00
[SIM] :07-29 15:43:36.115389:        :-:0x1:<0,1,0>:00 14 00 01 00 00 40 00 6c a4 0a 00 00 40 01 02
[SIM] :07-29 15:43:36.115421:        :-:0x1:<0,1,0>:03 04 00 00 00 00 00 00 00 00 00 00 00 00 00 00
[SIM] :07-29 15:43:36.115454:        :-:0x1:<0,1,0>:00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
```
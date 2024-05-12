## lucid_anony.dpt

This is a simplified, real-time packet anonymization program that anonymizes MAC addresses and IPv4 addresses based on user-specified parameters. It provides flexibility in determining which parts of the addresses to anonymize while preserving valuable prefix information. Packets, represented by the `eth_ip` event, arrive at the switch and trigger an `eth_ip` handler, which does two things: 

1) **Forwarding**: It sends a copy of the anonymized packet event out of a physical port on the switch.

2) **Basic Monitoring**: It generates a report displaying the packet before and after anonymization, and sends it to a monitoring server connected to a different port on the switch.

## Creating the Specification File

In order to run `lucid_anony.dpt` on a simulated network, you must first configure the specification json file, `lucid_anony.json`. To do so, you can run the provided python script, `pcapcoversion.py`. 
When script is run, it will convert the content of a provided pcap file (`smallflows.pcap` is provided, make sure to change the file being read in if you decide to use your own pcap) into a json called `lucid_anony.json` with each packet from the pcap being automatically formatted into a list of `eth_ip` events.
Once you have generated the json, you must manually add the table rules like this if you want to install table rules:

```
{
  "switches": 1,
  "max_time": 99999999,
  "events": [
    // Install table rules here  
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"anony_mac_dst_oui_tb", 
        "key":[0], 
        "action":"anony_mac_dst_oui_tb.hash_mac_action",
        "args": []
      }
    },
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"anony_mac_dst_id_tb", 
        "key":[1], 
        "action":"anony_mac_dst_id_tb.hash_mac_action",
        "args": []
      }
    },
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"anony_mac_src_oui_tb", 
        "key":[0], 
        "action":"anony_mac_src_oui_tb.hash_mac_action",
        "args": []
      }
    },
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"anony_mac_src_id_tb", 
        "key":[1], 
        "action":"anony_mac_src_id_tb.hash_mac_action",
        "args": []
      }
    },
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
          "table":"anony_src_ip_tb", 
          "key":["2198055104<<32>> &&& 4294967040<<32>>"],
          "action":"anony_src_ip_tb.get_ip_prefix", 
          "args":[4294967040]
      }
    },
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
          "table":"anony_dst_ip_tb", 
          "key":["2198055104<<32>> &&& 4294967040<<32>>"], 
          "action":"anony_dst_ip_tb.get_ip_prefix", 
          "args":[4294967040]
      }
    },
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
          "table":"anony_src_ip_tb", 
          "key":["33493164<<32>> &&& 4294901760<<32>>"],
          "action":"anony_src_ip_tb.get_ip_prefix", 
          "args":[4294901760]
      }
    },
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
          "table":"anony_dst_ip_tb", 
          "key":["33493164<<32>> &&& 4294901760<<32>>"], 
          "action":"anony_dst_ip_tb.get_ip_prefix", 
          "args":[4294901760]
      }
    },
    // Events simulating packets
    {
      "name": "eth_ip",
      "args": [
        6796,
        1440128,
        4219270,
        10154485,
        2048,
        4,
        5,
        0,
        983,
        30432,
        2,
        0,
        128,
        6,
        40572,
        2198055104,
        2329218632
      ]
    },
    {
      "name": "eth_ip",
      "args": [
        4219270,
        10154485,
        6796,
        1440128,
        2048,
        4,
        5,
        0,
        426,
        11618,
        0,
        0,
        52,
        6,
        30248,
        2329218632,
        2198055104
      ]
    },
    ...

      ]
  }
```
#### Usage

Finally, to run the program, make sure that you have docker and the lucid-docker image installed, then run `./lucid.sh interp .examples/apps/pkt_anony/lucid_anony.dpt` in the lucid home directory.

**Expected output**

Here's the expected output from `make interp`. We will break it down below.
```
dpt: -------Checking well-formedness---------
dpt: ---------typing1---------
dpt: ---------Concretizing symbolics-------------
dpt: ---------Aliasing Modules-------------
dpt: ---------Making returns explicit-------------
dpt: -----------renaming-----------
dpt: -------Performing parser slot analysis---------
dpt: -------Eliminating modules---------
dpt: -------Inlining size declarations---------
dpt: ---------typing2---------
dpt: -------Eliminating type aliases 2---------
dpt: -----------inlining functions-----------
dpt: -----------inlining tables-----------
dpt: ---------Eliminating events with global arguments----------
dpt: ---------------typing3-------------
dpt: -------Making user types concrete-------
dpt: ---------------typing4-------------
dpt: -------Eliminating vectors-------
dpt: ---------------typing5-------------
dpt: -----------re-renaming-----------
dpt: -------Eliminating EStmts-------
dpt: ---------------typing6-------------
dpt: -------Eliminating records-------
dpt: ---------------typing7-------------
dpt: -------Eliminating tuples-------
dpt: ---------------typing8-------------
dpt: -------Inlining Constants-------
dpt: -----------re-re-renaming-----------
dpt: ---------------typing again-------------
dpt: -------Translating to core syntax---------
dpt: -------Partial interpreting---------
dpt: Simulating...
dpt: Using random seed: 1715288726

t=8: Handling event eth_ip(6796,1440128,4219270,10154485,2048,4,5,0,983,30432,2,0,128,6,40572,2198055104,2329218632) at switch 0, port 0
t=608: Handling event prepare_report(6796,5072193,4219270,14530415,2048,4,5,0,983,30432,2,0,128,6,40572,2198054914,2329218632) at switch 0, port 196
Sending report about packet {src=2198054914; dst=2329218632} to monitor on port 2
+----------------------+
| Anonymized Packet    |
+----------------------+
| Flow Info:           |
|   dst_MAC_OUI: 6796 |
|   dst_MAC_ID:  5072193 |
|   src_MAC_OUI: 4219270 |
|   src_MAC_ID:  14530415 |
+----------------------+
| IP Addresses:        |
|   SRC IP: 2198054914 |
|   DST IP: 2329218632 |
+----------------------+

...

Drops :    [ ]

 packet events handled: 0
 total events handled: 9993

}
```

**reading the intepreter output**

The simulator's output for the provided `lucid_anony.json` is rather simple. Each packet event processed in the simulation has some associated trace that summarizes the event that was handled at each switch in the simulation respectively. In the case of this program, a single trace will look like so:
```
Sending report about packet {src=2198054914; dst=2329218632} to monitor on port 2
+----------------------+
| Anonymized Packet    |
+----------------------+
| Flow Info:           |
|   dst_MAC_OUI: 6796 |
|   dst_MAC_ID:  5072193 |
|   src_MAC_OUI: 4219270 |
|   src_MAC_ID:  14530415 |
+----------------------+
| IP Addresses:        |
|   SRC IP: 2198054914 |
|   DST IP: 2329218632 |
+----------------------+
```
The line `sending report...` as well as the `Anonymized Packet` table are printed via statements in the `prepare_report` handler. The `Anonymized Packet` simply provides the details of the anonymized `eth_ip` packet via a helper function that formats and prints the table. Additionally, another helper function, `Packet In`, exists and can be called before anonymization occurs in the `eth_ip` event if you are looking to see the details of the original packet in the trace.

## Anonymization Approach

Anonymization policies can be customized by modifying the entries in the `lucid_anony.json` file:
* MAC address anonymization is configured by installing `anony_mac_dst_oui_tb`, `anony_mac_dst_id_tb`, `anony_mac_src_oui_tb`, and `anony_mac_src_id_tb` table entries with hash_mac_action actions.
* IP address prefix-preserving anonymization is configured by installing `anony_src_ip_tb` and `anony_dst_ip_tb` entries specifying the number of prefix bits to preserve and a `get_ip_prefix` action

**Mac Addresses:**
 The user sets an argument value of 1 in the JSON file to indicate which MAC address parts (OUI and/or ID) to anonymize. When the program runs, the specified parts are hashed using a predefined hash seed to anonymize them.

For example, this JSON command installs a rule to anonymize the MAC destination OUI:
```json
{
  "type": "command", 
  "name":"Table.install", 
  "args":{
    "table":"anony_mac_dst_oui_tb", 
    "key":[1], 
    "action":"anony_mac_dst_oui_tb.hash_mac_action",
    "args": []
  }
}
```

The `hash_mac_action` constructor generates the hashing action:
```c
action_constr hash_mac_action() = {
    return action int<24> acn (int<24> mac_part) {
        return hash<24>(HASH_SEED, mac_part);
    };
};
```
**IPv4 Addresses:**
 IP prefix matching is performed using masks provided by the user as arguments. This allows the prefix to be preserved while the non-fixed part is extracted. The non-fixed part is then hashed to anonymize it. After, the anonymized non-fixed part is combined with the original prefix.

This JSON command installs a rule to anonymize the last 8 bits of the IPv4 source address:
```json
{
  "type": "command", 
  "name":"Table.install", 
  "args":{
      "table":"anony_src_ip_tb", 
      "key":["16909056<<32>> &&& 4294967040<<32>>"], 
      "action":"anony_src_ip_tb.get_ip_prefix", 
      "args":[4294967040]
  }
}
```
*"key": The used by both of the IP tables is a masked pattern. The user provides the IP containing the prefix in addition to the subnet for said prefix, then use the `&&&` operator to allow for any IPs with the same prefix to match. 
*"args": The install-time argument for the get_ip_prefix constructor. The user provides a install-time argument of a subnet for the IP prefix they are preserving, this subnet is then used to derive the prefix and host ID portions of a provided IP

The get_ip_prefix constructor generates the action to extract the prefix and non-fixed part:
```c
action_constr get_ip_prefix(int prefix_mask) = {
    return action prefix_output get_prefix_action(int ip) {
        return {prefix = ip & prefix_mask; non_fixed = ip & ~prefix_mask; subnet_mask = prefix_mask};
    };
};
```

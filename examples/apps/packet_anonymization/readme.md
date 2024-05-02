## lucid_anony.dpt

lucid_anony.dpt is a simplified packet anonymization program that anonymizes MAC addresses and IPv4 addresses based on user-specified parameters. It provides flexibility in determining which parts of the addresses to anonymize while preserving valuable prefix information. This program provides an example of how Lucid can be used to anonymize network packet traces to protect sensitive information while preserving utility for network analysis and security research.

### Anonymization Approach

Anonymization policies can be customized by modifying the entries in the `lucid_anony.json` file:
* MAC address anonymization is configured by installing `anony_mac_dst_oui_tb`, `anony_mac_dst_id_tb`, `anony_mac_src_oui_tb`, and `anony_mac_src_id_tb` table entries with hash_mac_action actions.
* IP address prefix-preserving anonymization is configured by installing `anony_src_ip_tb` and `anony_dst_ip_tb` entries specifying the number of prefix bits to preserve and a `get_ip_prefix` action

**Mac Addresses**
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
**IPv4 Addresses**
IP prefix matching is performed using masks provided by the user as arguments. This allows the prefix to be preserved while the non-fixed part is extracted. The non-fixed part is then hashed to anonymize it. After, the anonymized non-fixed part is combined with the original prefix.

This JSON command installs a rule to anonymize the last 8 bits of the IPv4 source address:
```json
{
  "type": "command", 
  "name":"Table.install", 
  "args":{
      "table":"anony_src_ip_tb", 
      "key":[16909056], 
      "mask": ["4294967040"],
      "action":"anony_src_ip_tb.get_ip_prefix", 
      "args":[4294967040, 255, 24]
  }
}
```

The get_ip_prefix constructor generates the action to extract the prefix and non-fixed part:
```c
action_constr get_ip_prefix(int prefix_mask, int non_fixed_mask, int prefix_size) = {
    return action prefix_output get_prefix_action(int ip) {
        return {prefix = ip & prefix_mask; non_fixed = ip & non_fixed_mask; fixed_length = prefix_size};
    };
};
```
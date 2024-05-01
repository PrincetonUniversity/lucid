## pivot.dpt

This is a simple pivot detecting application that runs on a switch. Packets, represented by the `eth_ip` event, arrive at the switch and trigger an `eth_ip` handler, which would check whether the packet is pivotting or not.
If it is pivotting, it would generate a report saying that the packet was caught, like this:

```
caught pivotting about flow {src=33554442; dst=16777226;} to monitor on port 2
```

and would block the packet.
I used several criteria to determine whether the packet is suspicioous or not. The main criteria is the hash of the TCP payload, along with size of the packet and the timestamp. Using these, the program goes through certain steps and if the packet is flagged multiple times, it would mark it as pivotting and block.

## Generating the json file

In order to generate the json file (pivot.json), you just have to run the python script called "pcapconversion2.py".
If you run this, it would convert all the packets inside the pcap file (jumphostssh.pcap) to json, and would create a json filed named "pivot.json" containing all the packets in the pcap file.
jumphostssh.pcap has packet captures of pivotting attacks in testing environment, so it is a good testcase to test this app.
One thing to note is that after you generate the json file, you need to manually add the table rules like this if you want to install table rules:

```
{
  "switches": 1,
  "max_time": 999999,
  "events": [
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"filter_table", 
        "key":[1], 
        "action":"filter_table.mk_result", 
        "args":["true"]}},
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"filter_table", 
        "key":[2], 
        "action":"filter_table.mk_result", 
        "args":["true"]}},
    {
      "type": "command", 
      "name":"Table.install", 
      "args":{
        "table":"filter_table", 
        "key":[50331658], 
        "action":"filter_table.mk_result", 
        "args":["true"]}},

    // Events    
    {
      "name": "eth_ip",
      "args": [
        11,
        22,
        2048,
        74,
        0,
        16777226,
        33554442,
        128
      ],
      "timestamp": 0
    },

    // Other events....
  ]
}

```


#### Usage

Finally, to run the program, make sure that you have docker and the lucid-docker image installed, then run `./lucid.sh interp ./newapps/pivotting/pivot.dpt` in the lucid home directory.

**Expected output**

Here's the snippet of expected output after running.
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
dpt: Using random seed: 1714600225

t=0: Handling event eth_ip(11,22,2048,74,0,16777226,33554442,128) at switch 0, port 0
t=1: Handling event eth_ip(11,22,2048,74,0,33554442,16777226,128) at switch 0, port 0
pszie is 74, prev size is 74, prev time is 0, and cur time is 1
pszie is 74, prev size is 0, prev time is 0, and cur time is 1
pszie is 74, prev size is 0, prev time is 0, and cur time is 1
flag: 1
t=2: Handling event eth_ip(11,22,2048,66,0,16777226,33554442,128) at switch 0, port 0
t=3: Handling event eth_ip(11,22,2048,107,759714643,16777226,33554442,128) at switch 0, port 0
t=4: Handling event eth_ip(11,22,2048,66,0,33554442,16777226,128) at switch 0, port 0
pszie is 66, prev size is 107, prev time is 3, and cur time is 4
pszie is 66, prev size is 0, prev time is 0, and cur time is 4
pszie is 66, prev size is 0, prev time is 0, and cur time is 4
flag: 0
t=5: Handling event eth_ip(11,22,2048,107,759714643,33554442,16777226,128) at switch 0, port 0
pszie is 107, prev size is 107, prev time is 3, and cur time is 5
pszie is 107, prev size is 0, prev time is 0, and cur time is 5
pszie is 107, prev size is 0, prev time is 0, and cur time is 5
flag: 1
t=6: Handling event eth_ip(11,22,2048,66,0,16777226,33554442,128) at switch 0, port 0
t=7: Handling event eth_ip(11,22,2048,1570,3691315200,16777226,33554442,128) at switch 0, port 0
t=8: Handling event eth_ip(11,22,2048,66,0,33554442,16777226,128) at switch 0, port 0
pszie is 66, prev size is 1570, prev time is 7, and cur time is 8
pszie is 66, prev size is 0, prev time is 0, and cur time is 8
pszie is 66, prev size is 0, prev time is 0, and cur time is 8

...


 Drops :    [ ]

 packet events handled: 0
 total events handled: 873

```

**reading the intepreter output**

What the program basically did is it took the "pivot.json" as the input, and processed all the packets.
If it caught something suspicious, it makes a report and blocks that packet.
Total events handled should be 873 based on "jumphostssh.pcap" file as the input, but you can actually change the threshold for the time and size which would change the totla events handled because it would produce less/more reports as you change.
One thing to note is that you might think what this line `pszie is 66, prev size is 0, prev time is 0, and cur time is 8` is.
This is just an output that I put for testing and debugging purposes, and I think it visualizes the flow well, so I just left it there. Same thing for `flag:`.



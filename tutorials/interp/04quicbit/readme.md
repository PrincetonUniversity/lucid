## Quic_RTT_App

This is a simple RTT measuring application that runs on a switch (given some modifications). Packets, represented by the 'quic_packet_alt' event, are processed to find the rtt if the spin bit was flipped or not. If the spin bit has been flipped, it will generate a report of the rtt similar to the below:

```
t=10: Handling event quic_packet_alt(1,1,false,1691864540) at switch 0, port 0
RTT for conversation 3 (spinbit true): 5970 ms
```
This program was created with the idea that the spin bit used in quic traffic could be used to measure rtt. Note that the valid edge counter is not implmented in this program. 

## Generating the JSON file:

In order to generate the json file, you have to run the pcapconversion_ipv6 script. By running this, all of the packets inside the pcap file (in this case the aioquic_spinbit_traffic.pcapng file) to json and creates a JSON file named quicbit4.json. This JSON file will contain all of hte packets that are within the pcap file. The output of the JSON file should look similar to the below:


```
{
    "max_time": 10000,
    "events": [
      {
        "name": "quic_packet_alt",
        "args": [
          1,
          1,
          false,
          1715383809645
        ]
      },
      {
        "name": "quic_packet_alt",
        "args": [
          1,
          1,
          false,
          1715383809656
        ]
      },
      {
        "name": "quic_packet_alt",
        "args": [
          1,
          1,
          false,
          1715383809657
        ]
      },

      // Other events here

    ]
}


```

## How to use this program

To run this program, make sure that both docker and the lucid-docker image are installed. Afterwards, run the command './lucid.sh interp ./tutorials/interp/04quicbit/quicbit4.dpt --suppress-final-state' from the lucid home directory. If you want to get more output, you can run the command without the --suppress-final-state portion. 

**Expected Output**

Here is an expected snippet of expected output after running the above command:

```
program: ./tutorials/interp/04quicbit/quicbit4.dpt
sources: ./tutorials/interp/04quicbit/quicbit4.dpt
no interp spec file provided
checking if spec exists: ./tutorials/interp/04quicbit/quicbit4.json
spec exists, adding args
running command: docker run --rm -it  --mount type=bind,source=/data/brr2tu/lucid/tutorials/interp/04quicbit/quicbit4.dpt,target=/app/inputs/quicbit4.dpt --mount type=bind,source=/data/brr2tu/lucid/tutorials/interp/04quicbit/quicbit4.json,target=/app/inputs/quicbit4.json jsonch/lucid:lucid /bin/sh -c "./dpt /app/inputs/quicbit4.dpt --spec /app/inputs/quicbit4.json"
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
dpt: Using random seed: 1715627762

t=0: Handling event quic_packet_alt(1,1,false,1691858541) at switch 0, port 0
t=1: Handling event quic_packet_alt(1,1,false,1691858552) at switch 0, port 0
t=2: Handling event quic_packet_alt(1,1,false,1691858553) at switch 0, port 0
t=3: Handling event quic_packet_alt(1,1,false,1691858554) at switch 0, port 0
t=4: Handling event quic_packet_alt(1,1,false,1691858556) at switch 0, port 0
t=5: Handling event quic_packet_alt(1,1,false,1691858570) at switch 0, port 0
t=6: Handling event quic_packet_alt(1,1,true,1691858570) at switch 0, port 0
RTT for conversation 3 (spinbit true): 0 ms
t=7: Handling event quic_packet_alt(1,1,true,1691858570) at switch 0, port 0
RTT for conversation 3 (spinbit true): 0 ms
t=8: Handling event quic_packet_alt(1,1,false,1691858574) at switch 0, port 0
RTT for conversation 3 (spinbit true): 4 ms
t=9: Handling event quic_packet_alt(1,1,false,1691858576) at switch 0, port 0
RTT for conversation 3 (spinbit true): 6 ms
t=10: Handling event quic_packet_alt(1,1,false,1691864540) at switch 0, port 0
RTT for conversation 3 (spinbit true): 5970 ms
t=11: Handling event quic_packet_alt(1,1,false,1691864550) at switch 0, port 0
RTT for conversation 3 (spinbit true): 5980 ms
t=12: Handling event quic_packet_alt(1,1,true,1691864551) at switch 0, port 0
RTT for conversation 3 (spinbit true): 1 ms
t=13: Handling event quic_packet_alt(1,1,true,1691864552) at switch 0, port 0
RTT for conversation 3 (spinbit true): 2 ms
t=14: Handling event quic_packet_alt(1,1,true,1691864553) at switch 0, port 0
RTT for conversation 3 (spinbit true): 3 ms
t=15: Handling event quic_packet_alt(1,1,true,1691864553) at switch 0, port 0
RTT for conversation 3 (spinbit true): 3 ms
t=16: Handling event quic_packet_alt(1,1,true,1691864554) at switch 0, port 0
RTT for conversation 3 (spinbit true): 4 ms
t=17: Handling event quic_packet_alt(1,1,true,1691864555) at switch 0, port 0
RTT for conversation 3 (spinbit true): 5 ms
t=18: Handling event quic_packet_alt(1,1,false,1691867558) at switch 0, port 0
RTT for conversation 3 (spinbit true): 3003 ms
t=19: Handling event quic_packet_alt(1,1,false,1691867569) at switch 0, port 0
RTT for conversation 3 (spinbit true): 3014 ms
t=20: Handling event quic_packet_alt(1,1,true,1691867571) at switch 0, port 0
RTT for conversation 3 (spinbit true): 2 ms
t=21: Handling event quic_packet_alt(1,1,true,1691867589) at switch 0, port 0
RTT for conversation 3 (spinbit true): 20 ms
t=22: Handling event quic_packet_alt(1,1,false,1691867589) at switch 0, port 0
RTT for conversation 3 (spinbit true): 0 ms
t=23: Handling event quic_packet_alt(1,1,true,1691867592) at switch 0, port 0
RTT for conversation 3 (spinbit true): 3 ms
t=24: Handling event quic_packet_alt(1,1,true,1691867592) at switch 0, port 0
RTT for conversation 3 (spinbit true): 3 ms
t=25: Handling event quic_packet_alt(1,1,true,1691867605) at switch 0, port 0
RTT for conversation 3 (spinbit true): 16 ms
t=26: Handling event quic_packet_alt(1,1,true,1691867605) at switch 0, port 0
RTT for conversation 3 (spinbit true): 16 ms
t=27: Handling event quic_packet_alt(1,1,false,1691867606) at switch 0, port 0
RTT for conversation 3 (spinbit true): 1 ms
t=28: Handling event quic_packet_alt(1,1,false,1691867607) at switch 0, port 0
RTT for conversation 3 (spinbit true): 2 ms
t=29: Handling event quic_packet_alt(1,1,true,1691867608) at switch 0, port 0
RTT for conversation 3 (spinbit true): 1 ms
t=30: Handling event quic_packet_alt(1,1,false,1691867608) at switch 0, port 0
RTT for conversation 3 (spinbit true): 0 ms

```

**What this output means**

The program, as stated before, took "quicbit4.json" as the input, and processed all of the packets. If the spin bit was flipped, then the rtt for that conversation was reported. The total events handled for the json file provided should be 31. 


## Other Important Notes 

QUIC traffic that uses the spin bit is not typically available on the internet, as noted by https://dl.acm.org/doi/10.1145/3618257.3624844. In order to obtain our quic traffic, I used the aioquic library at https://github.com/aiortc/aioquic and used their examples to generate quic traffic. Wireshark was then used to capture the QUIC traffic and then converted into a pcapng file. 

One other note is that for actual usage with a switch, the program would need to be edited in order to get the current time that the switch has, not the time from within the packet itself. The main change would be getting rid of the int cur_time argument, and changing the int current_ts variable to equal Sys.time() on lines 15 and 17, respectively. Another change that would have to be made is to the pcapconversion_ipv6.py file. In that file, you would remove the timestamp variable on line 16 and then get rid of the corresponding timestamp variable on line 47 in the '"args": [src, dst, spin_flipped, timestamp]' line. The script would also need to be rerun in order to create a JSON file that matches the event. 





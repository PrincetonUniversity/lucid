'''
Assume we have one entry and one exit event, declared as:
entry event packetin(int srcIP, int dstIP, int inport);
exit event packetout(int outport);
'''

# Python bindings for OCaml: https://github.com/thierry-martinez/pyml

# We could possibly make the entire topology available to the python program,
# although that seems a bit weird since it's not in charge of actually moving
# packets around.

# Parse a non-event packet into an event packet (or None if it should be dropped)
# location should be a pair of switch and port number
# packet is just the packet, represented as an object
def parse(location, packet):
    # The location lets you parse differently at different points in the network
    if (location == (1, 10)):
        return None # For example

    try:
        if(packet["IP"] == true):
            packet["Event"] = {"name": "packetin", "args": [packet["src"], packet["dst"], location[1]]}
            return packet
    except e: # e.g. if one of the dictionary lookups fails
        pass

    return None

# Turn an exit event into a non-event packet  and location (or None if it should just be logged)
# Note that the packet passed here is the input packet itself, but with the extra
# Event header attached
def deparse(location, packet):
    try:
        if(packet["Event"]["name"] == "packetout"):
            out_port = packet["Event"]["args"][0]
            out_packet = dict(packet) # Avoid mutability. Probably not actually an issue
            del out_packet["Event"] # Remove the event header
            # We could also modify the packet in other ways, e.g. change the dst
            # address in a load balancer or something, if we wanted to.
            return (location[0], out_port), out_packet
    except e:
        pass

    return None

'''
Questions for the group:
- Should links always be bidirectional?
- In the case of exit events which leave the network (i.e. go out a port that doesn't
  appear in the topology), should we log the exit event or the deparsed packet?
- We can use the config topology to automatically fill certain builtin lucid variables,
  e.g. neighbors. What else might we want to build automatically? We could also do e.g.
  each subset of neighbors with exactly one neighbor missing (for flooding), a list of
  all switches and links in the network, etc.
'''

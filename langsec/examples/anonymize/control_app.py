import subprocess

PATH = "langsec/examples/anonymize/"
LUCID_FILE = "anonymize.dpt"
OUT_FILE = "output.txt"
IN_FILE = "input.txt"
POLICY_FILE = "policy.txt"
EVENT_FILE = "events.txt"

def parse_policy(policy_file):
    rules = []

    # parse file containing anonymization policy
    file = open(policy_file, "r")
    policy_args = "["
    ip_prefixes = []
    for line in file.readlines():
        line = line.split(":")[-1].strip().lower()
        if line == "yes":
            policy_args += "\"true\","
        elif line == "no":
            policy_args += "\"false\","
        else:
            ip_prefixes.append(line)
    policy_args = policy_args[:-1] + "]"

    TABLE_INSTALL = "{\"type\":\"command\",\"name\":\"Table.install\",\"args\":"
    TABLE_ARGS = "{\"table\":\"%s\",\"key\":[%s],\"action\":\"%s\",\"args\":%s}"

    # create command to install policy rule
    POLICY_TABLE = "anonymizer.policy_table"
    POLICY_ACTION = "anonymizer.policy_table.Anonymizer_get_policy"
    add_policy = TABLE_INSTALL + TABLE_ARGS % (POLICY_TABLE, 1, POLICY_ACTION, policy_args) + "}"
    rules.append(add_policy)

    # create command to install IP prefix-matching rules
    PREFIX_TABLE = "anonymizer.%s_prefix_table.prefix_table"
    PREFIX_ACTION = "anonymizer.%s_prefix_table.prefix_table.PrefixTable_result"
    def create_prefix_table_rules(prefixes, ip_addr_type):
        table = PREFIX_TABLE % ip_addr_type
        action = PREFIX_ACTION % ip_addr_type
        prefixes = prefixes[1:-1].split(",")
        for prefix in prefixes:
            prefix_info = prefix.strip().split("/") 
            prefix_octets = prefix_info[0].split(".")
            prefix_len = int(prefix_info[1])
            for i in range(prefix_len // 8, 4):
                prefix_octets[i] = "\"_\""

            prefix_key = ""
            for octet in prefix_octets:
                prefix_key += octet + ","
            prefix_key = prefix_key[:-1]
            
            add_prefix_match = TABLE_INSTALL + TABLE_ARGS % (table, prefix_key, action, "[%s]" % prefix_len) + "}"
            rules.append(add_prefix_match)
            
    create_prefix_table_rules(ip_prefixes[0], "src")
    create_prefix_table_rules(ip_prefixes[1], "dst")

    return rules

def add_event(eth, ip, event_file):
    with open(PATH + event_file, "a") as file:
        PACKET_EVENT = "{\"name\":\"eth_ip\", \"args\": %s}"
        eth_list = [eth["dmac"], eth["smac"], eth["etype"]]
        ip_list = [ip["src"], ip["dst"], ip["len"]]
        packet_args = eth_list + ip_list
        file.write("\n" + PACKET_EVENT % packet_args)
        file.close()
 
def anonymize():
    rules = "\n".join(parse_policy(PATH + POLICY_FILE))
    events = open(PATH + EVENT_FILE, "r").read()
    cmd = ["./dpt", "-i", PATH + LUCID_FILE]
    with open(PATH + IN_FILE, "w") as in_file:
        in_file.write(rules + "\n" + events)
        in_file.close()

    with open(PATH + OUT_FILE, "w") as out_file:
        try:
            process = subprocess.run(cmd, stdin=open(PATH + IN_FILE, "r"), stdout=out_file, timeout=10)
        except subprocess.TimeoutExpired:
            pass
        out_file.close()

eth = {"dmac": 11, "smac": 22, "etype": 2048}
ip = {"src": 13, "dst": 14, "len": 128}
add_event(eth, ip, EVENT_FILE)
anonymize()


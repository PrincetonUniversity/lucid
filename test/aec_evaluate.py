import subprocess, os, filecmp

moduleFiles = [
"BloomFilter",
"BloomFilterTimeout",
"Hashtable",
"HashtableCuckoo",
"HashtableTimeout",
"HashtableCuckooTimeout",
"Bimap",
"CountMinSketch",
"CountMinSketchTimeout"
]

applicationFiles = [
"stateful_fw",
"dnsguard",
"starflow",
"chain_prob_stateful_firewall",
"chain_prob_stateful_firewall_timeout",
"NAT",
"countmin_historical"
]

errors = []

print("Running tests on module files")
for file in moduleFiles:
    print("\n"+file, flush=True)
    fullfile = "examples/popl22/"+file+".dpt"
    cmd = ["./dpt", "--evaluate", fullfile]
    ret = subprocess.run(cmd)
    if ret.returncode != 0:
        errors.append(file)

print("\nRunning tests on application files")
for file in applicationFiles:
    print("\n"+file, flush=True)
    fullfile = "examples/popl22/"+file+".dpt"
    cmd = ["./dpt", "--evaluate", fullfile]
    ret = subprocess.run(cmd)
    if ret.returncode != 0:
        errors.append(file)

print("\nErrors (should be empty):", errors)

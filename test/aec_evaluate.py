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

outfile = open("test/aec_output.txt", "w")

outfile.write("Running tests on module files")
for file in moduleFiles:
    outfile.write("\n"+file+" ")
    outfile.flush()
    fullfile = "examples/popl22/"+file+".dpt"
    cmd = ["./dpt", "--evaluate", fullfile]
    ret = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(file)

outfile.write("\nRunning tests on application files")
for file in applicationFiles:
    outfile.write("\n"+file+" ")
    outfile.flush()
    fullfile = "examples/popl22/"+file+".dpt"
    cmd = ["./dpt", "--evaluate", fullfile]
    ret = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(file)

outfile.write("\nErrors (should be empty): "+str(errors))

outfile.close()

print("Done with evaluation. Output can be found in test/aec_output.txt.")

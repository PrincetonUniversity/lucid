import subprocess, os, filecmp

testfiles = [
"basics",
"NAT",
"chain_stateful_firewall",
"match",
"global_args",
"return_test",
"user_types",
"module",
"operators"
]

libraryfiles = [x for x in os.listdir("examples/library/") if x.endswith(".dpt")]

errors = []
diffs = []

if not (os.path.isdir("test/output")):
    os.mkdir("test/output")

for file in testfiles:
    print("Running test on "+file)
    outname = "{}_output.txt".format(file)
    with open("test/output/"+outname, "w") as outfile:
        fullfile = "examples/interp_tests/"+file+".dpt"
        cmd = ["./dpt", fullfile]
        ret = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(file)
    elif not filecmp.cmp("test/output/"+outname, "test/expected/"+outname):
        diffs.append(file)
    outfile.close()

for file in libraryfiles:
    print("Typechecking "+file)
    cmd = ["./dpt", fullfile]
    ret = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(file)

print("Errors:", errors)
print("Diffs:", diffs)

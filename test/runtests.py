import subprocess, os, filecmp

interpfiles = [
"basics",
"NAT",
"chain_stateful_firewall",
"match",
"global_args",
"return_test",
"user_types",
"module",
"operators",
"BloomFilter",
"symbolics",
"symbolics0"
]

libraryfiles = [x for x in os.listdir("examples/library/") if x.endswith(".dpt")]
regressionfiles = [x for x in os.listdir("examples/regression/") if x.endswith(".dpt")]
poplfiles = [x for x in os.listdir("examples/popl22/") if x.endswith(".dpt")]

errors = []
diffs = []

if not (os.path.isdir("test/output")):
    os.mkdir("test/output")

def interp_test(file, args):
    print("Running test on "+file)
    outname = "{}_output.txt".format(file)
    with open("test/output/"+outname, "w") as outfile:
        fullfile = "examples/interp_tests/"+file+".dpt"
        cmd = ["./dpt", "--silent", fullfile] + args
        ret = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(file)
    elif not filecmp.cmp("test/output/"+outname, "test/expected/"+outname):
        diffs.append(file)
    outfile.close()

def just_typecheck(path, file, suffix = ""):
    print("Typechecking "+file)
    fullfile = path+file+suffix
    cmd = ["./dpt", "--silent",  fullfile]
    ret = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(file)


for file in interpfiles:
    interp_test(file, [])

for file in libraryfiles:
    just_typecheck("examples/library/", file)

for file in regressionfiles:
    just_typecheck("examples/regression/", file)

for file in poplfiles:
    just_typecheck("examples/popl22/", file)

print("Errors:", errors)
print("Diffs:", diffs)

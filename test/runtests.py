import subprocess, os, filecmp

interpfiles = [x for x in os.listdir("examples/interp_tests/") if x.endswith(".dpt")]
libraryfiles = [x for x in os.listdir("examples/library/") if x.endswith(".dpt")]
regressionfiles = [x for x in os.listdir("examples/regression/") if x.endswith(".dpt")]
poplfiles = [x for x in os.listdir("examples/popl22/") if x.endswith(".dpt")]

errors = []
diffs = []

if not (os.path.isdir("test/output")):
    os.mkdir("test/output")

def interp_test(fullfile, args):
    shortfile = fullfile[0:-4]
    print("Running test on "+shortfile)
    outname = "{}_output.txt".format(shortfile)
    with open("test/output/"+outname, "w") as outfile:
        fullfile = "examples/interp_tests/"+fullfile
        cmd = ["./dpt", "--silent", fullfile] + args
        ret = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(fullfile)
    elif not filecmp.cmp("test/output/"+outname, "test/expected/"+outname):
        diffs.append(shortfile)
    outfile.close()

def just_typecheck(path, file, suffix = ""):
    print("Typechecking "+file)
    fullfile = path+file+suffix
    cmd = ["./dpt", "--silent",  fullfile]
    ret = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if ret.returncode != 0:
        errors.append(fullfile)


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

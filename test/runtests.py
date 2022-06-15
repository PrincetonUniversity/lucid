import subprocess, os, filecmp

interpdir = "examples/interp_tests/"
librarydir = "examples/library/"
regressiondir = "examples/misc/regression/"
popldir = "examples/publications/popl22/"

interpfiles = [x for x in os.listdir(interpdir) if x.endswith(".dpt")]
libraryfiles = [x for x in os.listdir(librarydir) if x.endswith(".dpt")]
regressionfiles = [x for x in os.listdir(regressiondir) if x.endswith(".dpt")]
poplfiles = [x for x in os.listdir(popldir) if x.endswith(".dpt")]

errors = []
diffs = []

if not (os.path.isdir("test/output")):
    os.mkdir("test/output")

def interp_test(fullfile, args):
    shortfile = fullfile[0:-4]
    print("Running test on "+shortfile)
    outname = "{}_output.txt".format(shortfile)
    with open("test/output/"+outname, "w") as outfile:
        fullfile = interpdir+fullfile
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
    just_typecheck(librarydir, file)

for file in regressionfiles:
    just_typecheck(regressiondir, file)

for file in poplfiles:
    just_typecheck(popldir, file)

print("Errors:", errors)
print("Diffs:", diffs)

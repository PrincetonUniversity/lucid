import subprocess, os, filecmp

interpdir = "examples/interp_tests/"
librarydir = "examples/library/"
regressiondir = "examples/misc/regression/"
parserdir = "examples/misc/parsers/"
popldir = "examples/publications/popl22/"

interactivefiles = [x for x in os.listdir(interpdir) if x.endswith("staticrouter.dpt")]
interpfiles = [x for x in os.listdir(interpdir) if ((x.endswith(".dpt")) and (x not in interactivefiles))]
libraryfiles = [x for x in os.listdir(librarydir) if x.endswith(".dpt")]
regressionfiles = [x for x in os.listdir(regressiondir) if x.endswith(".dpt")]
parserfiles = [x for x in os.listdir(parserdir) if x.endswith(".dpt")]
poplfiles = [x for x in os.listdir(popldir) if x.endswith(".dpt")]

errors = []
bad_successes = []
diffs = []

if not (os.path.isdir("test/output")):
    os.mkdir("test/output")

# Convention: a test is expected to fail if and only if the test file ends in
# _wrong.dpt
def check_return(ret, fullfile):
    expect_error = fullfile.endswith("_wrong.dpt")
    if ret.returncode != 0 and (not expect_error):
         print("test returned error: "+"./dpt --silent %s"%fullfile)
         errors.append(fullfile)
    if ret.returncode == 0 and expect_error:
        print("test unexpectedly did NOT return error: "+"./dpt --silent %s"%fullfile)
        bad_successes.append(fullfile)

def interp_test(fullfile, args):
    shortfile = fullfile[0:-4]
    print("Running test on "+shortfile)
    outname = "{}_output.txt".format(shortfile)
    with open("test/output/"+outname, "w") as outfile:
        fullfile = interpdir+fullfile
        cmd = ["./dpt", "--silent", fullfile] + args
        ret = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL)
    check_return(ret, fullfile)
    if not filecmp.cmp("test/output/"+outname, "test/expected/"+outname):
        print("test returned different output than expected: "+"./dpt --silent %s"%fullfile)
        diffs.append(shortfile)
    outfile.close()

def just_typecheck(path, file, suffix = ""):
    print("Typechecking "+file)
    fullfile = path+file+suffix
    cmd = ["./dpt", "--silent",  fullfile]
    ret = subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    check_return(ret, fullfile)


def interactive_test(fullfile, args):
    shortfile = fullfile[0:-4]
    print("Running interactive test on "+shortfile)
    outname = "{}_output.txt".format(shortfile)
    with open("test/output/"+outname, "w") as outfile:
        fullfile = interpdir+fullfile
        input_events_fn = "%s.input.txt"%fullfile
        cmd = ["./dpt", "-i", fullfile] + args
        try:
            # interactive mode always waits for more input, so this should timeout
            ret = subprocess.run(cmd, stdin=open(input_events_fn, "r"),stdout=outfile, stderr=subprocess.DEVNULL, timeout=3)
        except subprocess.TimeoutExpired:
            pass
        if not filecmp.cmp("test/output/"+outname, "test/expected/"+outname):
            diffs.append(shortfile)
        outfile.close()

for file in interpfiles: interp_test(file, [])

for file in libraryfiles: just_typecheck(librarydir, file)

for file in regressionfiles: just_typecheck(regressiondir, file)

for file in parserfiles: just_typecheck(parserdir, file)

for file in poplfiles: just_typecheck(popldir, file)

for file in interactivefiles: interactive_test(file, [])

print("Diffs:", diffs)
print("Unexpected error:", errors)
print("Unexpected success:", bad_successes)

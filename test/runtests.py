import subprocess, os, filecmp, sys

"""
This script is a simple test harness for the lucid interpreter. 
If you are adding an application as a test case, you should: 
    1. generate an expected output file by running `dpt --silent appname.dpt > appname_output.txt`
        and saving the output to the "test/expected" directory. 
        NOTE: please make sure that the output file does not have the same name as any already 
        existing output files in the "test/expected" directory.
    2. modify the "appfiles" list to include the path to your .dpt file
        note: you should have a corresponding <appname>.json file in the same directory
        that has a test case in it.
    3. Run this script by running `make test` in the root repo directory. Make sure your test case 
       passes. 
       NOTE: A common failure is non-deterministic output. Make sure to specify the seed in the 
       json test case file to prevent this. 
"""

# (C backend tests: test/lucidcc)
if len(os.sys.argv) > 1:
    print("Unrecognized argument: "+os.sys.argv[1])
    os.sys.exit(1)


interpdir = "examples/interp_tests/"
librarydir = "examples/library/"
regressiondir = "examples/misc/regression/"
parserdir = "examples/misc/parsers/"
popldir = "examples/publications/popl22/"

interactivefiles = [x for x in os.listdir(interpdir) if x.endswith("staticrouter.dpt")]
interpfiles = [interpdir + x for x in os.listdir(interpdir) if ((x.endswith(".dpt")) and (x not in interactivefiles))]
libraryfiles = [x for x in os.listdir(librarydir) if x.endswith(".dpt")]
regressionfiles = [x for x in os.listdir(regressiondir) if x.endswith(".dpt")]
parserfiles = [x for x in os.listdir(parserdir) if x.endswith(".dpt")]
poplfiles = [x for x in os.listdir(popldir) if x.endswith(".dpt")]

# add new app test cases here. Specify the .dpt file to run 
# with its directory relative to the repo root.
appfiles = [
    "examples/apps/pivotting_app/pivot.dpt",
    "examples/apps/pkt_anonymizer/lucid_anony.dpt",
    "examples/apps/quicbit/quicbit4.dpt"    
]

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
    shortfile = os.path.splitext(os.path.basename(fullfile))[0]
    print("Running test on "+shortfile + " ("+fullfile+")")
    outname = "{}_output.txt".format(shortfile)
    with open("test/output/"+outname, "w") as outfile:
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


print("--- interpreter tests ---")
for file in interpfiles: interp_test(file, [])

print("--- library tests ---")
for file in libraryfiles: just_typecheck(librarydir, file)

print("--- interactive interpreter tests ---")
for file in interactivefiles: interactive_test(file, [])

print("--- other tests ---")
for file in regressionfiles: just_typecheck(regressiondir, file)

for file in parserfiles: just_typecheck(parserdir, file)

for file in poplfiles: just_typecheck(popldir, file)

print("--- application tests ---")
for file in appfiles: interp_test(file, [])

print("--- p4 bmv2 example tests ---")
bmv2_test = "examples/p4_bmv2_examples/test.py"
bmv2_ret = subprocess.run([sys.executable, bmv2_test])
if bmv2_ret.returncode != 0:
    diffs.append("p4_bmv2_examples")


print("Diffs:", diffs)
print("Unexpected error:", errors)
print("Unexpected success:", bad_successes)

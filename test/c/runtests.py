#! /usr/bin/env python3
import subprocess, os, filecmp
"""lucidcc test script"""

test_dir = os.path.dirname(os.path.abspath(__file__))
def find_repo_root(start_dir, filename):
    current_dir = start_dir
    while True:
        file_list = os.listdir(current_dir)
        parent_dir = os.path.dirname(current_dir)
        if filename in file_list:
            return current_dir
        elif current_dir == parent_dir:  # If we've reached the root
            return None
        current_dir = parent_dir
def find_git_repo_root():
    try:
        root_dir = subprocess.check_output(["git", "rev-parse", "--show-toplevel"])
        return root_dir.decode('utf-8').strip()
    except subprocess.CalledProcessError:
        return None
root_dir = find_repo_root(test_dir, "dpt.opam")
if root_dir is None:
    root_dir = find_git_repo_root()
if root_dir is None:
    print("Could not find root directory of the repository")
    exit(1)
print("Root directory of the repository: " + root_dir)
os.chdir(root_dir)

# parse a single command line argument: "--lucidcc" to test the c backend, otherwise test interpreter
test_tgt = "lucidcc"

interpdir = "examples/interp_tests/"
librarydir = "examples/library/"
regressiondir = "examples/misc/regression/"
parserdir = "examples/misc/parsers/"
popldir = "examples/publications/popl22/"
output_dir = test_dir+"/output/"
expected_dir = test_dir+"/expected/"

interactivefiles = [x for x in os.listdir(interpdir) if x.endswith("staticrouter.dpt")]
interpfiles = [x for x in os.listdir(interpdir) if ((x.endswith(".dpt")) and (x not in interactivefiles))]
libraryfiles = [x for x in os.listdir(librarydir) if x.endswith(".dpt")]
regressionfiles = [x for x in os.listdir(regressiondir) if x.endswith(".dpt")]
parserfiles = [x for x in os.listdir(parserdir) if x.endswith(".dpt")]
poplfiles = [x for x in os.listdir(popldir) if x.endswith(".dpt")]

errors = []
bad_successes = []
diffs = []



if not (os.path.isdir(output_dir)):
    os.mkdir(output_dir)

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

def check_lucidcc_compat(incompat_keywords, fullfile):
    fname = fullfile[0:-4]
    outname = "{}.c".format(fname)
    inname = interpdir+fullfile
    # use a regex to check if fillfile has an incompat_keyword in it
    with open(inname, "r") as infile:
        for line in infile:
            for keyword in incompat_keywords:
                if keyword in line:
                    return keyword
    return None

def lucidcc_test(n_tests, i, fullfile, args):
    incompat_keywords = ["Counter.create", "PairArray.create", "Payload.t"]
    incompat = check_lucidcc_compat(incompat_keywords, fullfile)
    if incompat != None:
        print ("skipping lucidcc test on "+fullfile+" because it contains an incompatible feature: "+str(incompat))
        return
    if "wrong" in fullfile:
        print ("skipping lucidcc test on "+fullfile+" because it is expected to fail")
        return
    print ("Running lucid cc test {}/{} on {}".format(str(i), str(n_tests), fullfile))
    fname = fullfile[0:-4]
    outname = "{}.c".format(fname)
    inname = interpdir+fullfile
    cmd = ["./lucidcc", inname, "-o", output_dir+outname] + args
    # print("running command: {}".format(" ".join(cmd)))
    ret = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if ret.returncode != 0: 
        print("----- test failed -----")
        print("----- stdout -----")
        print(ret.stdout.decode("utf-8"))
        print("----- stderr -----")
        print(ret.stderr.decode("utf-8"))
        print("failed command: ")
        print(" ".join(cmd))
        exit(1)
if __name__ == "__main__":
    print("testing lucid c compiler")
    n_tests = len(interpfiles)
    for i, file in enumerate(interpfiles): lucidcc_test(n_tests, i, file, ["--debug"])
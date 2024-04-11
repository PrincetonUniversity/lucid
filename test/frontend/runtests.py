import subprocess, os, filecmp
"""frontend and lucid interpreter test script"""

# parse a single command line argument: "--lucidcc" to test the c backend, otherwise test interpreter
test_tgt = "interpreter"
if len(os.sys.argv) > 1:
    if os.sys.argv[1] == "--lucidcc":
        test_tgt = "lucidcc"
    else:
        print("Unrecognized argument: "+os.sys.argv[1])
        os.sys.exit(1)

interpdir = "examples/interp_tests/"
librarydir = "examples/library/"
regressiondir = "examples/misc/regression/"
parserdir = "examples/misc/parsers/"
popldir = "examples/publications/popl22/"
test_dir = os.path.dirname(os.path.abspath(__file__))
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

def interp_test(fullfile, args):
    shortfile = fullfile[0:-4]
    print("Running test on "+shortfile)
    outname = "{}_output.txt".format(shortfile)
    with open(output_dir+outname, "w") as outfile:
        fullfile = interpdir+fullfile
        cmd = ["./dpt", "--silent", fullfile] + args
        ret = subprocess.run(cmd, stdout=outfile, stderr=subprocess.DEVNULL)
    check_return(ret, fullfile)
    if not filecmp.cmp(output_dir+outname, expected_dir+outname):
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
    with open(output_dir+outname, "w") as outfile:
        fullfile = interpdir+fullfile
        input_events_fn = "%s.input.txt"%fullfile
        cmd = ["./dpt", "-i", fullfile] + args
        try:
            # interactive mode always waits for more input, so this should timeout
            ret = subprocess.run(cmd, stdin=open(input_events_fn, "r"),stdout=outfile, stderr=subprocess.DEVNULL, timeout=3)
        except subprocess.TimeoutExpired:
            pass
        if not filecmp.cmp(output_dir+outname, expected_dir+outname):
            diffs.append(shortfile)
        outfile.close()

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
    cmd = ["./lucidcc", inname, "-o", "test/ccoutput/"+outname] + args
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
    # fulloutfile = 
    # cmd = ["./lucidcc", fullfile, "-o", outname] + args
    # ret = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    # stdout_str = ret.stdout.decode("utf-8")
    # check_return(ret, fullfile)
    # if not filecmp.cmp(output_dir+outname, expected_dir+outname):
    #     print("test returned different output than expected: "+"./dpt --silent %s"%fullfile)
    #     diffs.append(shortfile)
    # outfile.close()

if (test_tgt == "interpreter"):
    for file in interpfiles: interp_test(file, [])

    for file in libraryfiles: just_typecheck(librarydir, file)

    for file in regressionfiles: just_typecheck(regressiondir, file)

    for file in parserfiles: just_typecheck(parserdir, file)

    for file in poplfiles: just_typecheck(popldir, file)

    for file in interactivefiles: interactive_test(file, [])

    print("Diffs:", diffs)
    print("Unexpected error:", errors)
    print("Unexpected success:", bad_successes)

elif (test_tgt == "lucidcc"):
    if not (os.path.isdir("test/ccoutput")):
        os.mkdir("test/ccoutput")

    print("testing lucid compiler")
    n_tests = len(interpfiles)
    for i, file in enumerate(interpfiles): lucidcc_test(n_tests, i, file, ["--debug"])
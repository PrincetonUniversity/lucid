#!/usr/bin/env python3
""" Run compile / assemble / execute tests. """

usage = "./compilertests.py all_tests.json"

import os, sys, errno, shutil, subprocess, re, json, itertools
from sys import path
from pathlib import Path


def main():

    dptc = find_file("dptc")
    if (len(sys.argv) != 2):        
        print ("incorrect args. usage: "+usage)
        return
    srcs_path = os.path.dirname(os.path.realpath(sys.argv[1]))
    tests_to_run = json.load(open(sys.argv[1], "r"))
    builds_path = tests_to_run["builds_dir"]
    tests = tests_to_run["tests"]
    all_cmds =  list(itertools.chain(*[t["commands"] for t in tests]))
    print ("sources directory: %s"%srcs_path)
    print ("builds directory: %s"%builds_path)
    print ("**** running %s tests ****"%(len(all_cmds)))
    os.makedirs(builds_path, exist_ok=True)
    results = []
    for test in tests:
        results += run_test(srcs_path, builds_path, dptc, test)
    summarize(results)
    return
    # cleanup(builds_dir, results)

def cmd_str(command, prog):
    return "%s %s"%(command, prog)

def start_str(command, prog):
    return "[start] "+cmd_str(command, prog)

def finish_str(command, prog, result):
    if (result):
        return (blue("[PASS] ")+cmd_str(command, prog))
    else:
        return (red("[FAIL] ")+cmd_str(command, prog))
        
def run_test(srcs_path, builds_dir, dptc, test):
    commands = test["commands"]
    results = []
    test_failed = False    
    for command in commands:  
        result = False                  
        if (test_failed):
            print (red("[FAIL: prior command failed] ")+cmd_str(command, test["prog"]))
            result = False
        else:
            print (start_str(command, test["prog"]))
            if (command == "compile"):
                prog = srcs_path + "/" + test["prog"]
                harness = srcs_path + "/" + test["harness"]
                build = builds_dir + "/" + test["build"]
                result = compile(dptc, prog, harness, build)
            elif (command == "assemble"):
                build = builds_dir + "/" + test["build"]
                result = assemble(build)
            elif (command == "execute"):
                build = builds_dir + "/" + test["build"]
                testspec = srcs_path + "/" + test["testspec"]
                result = execute(build, testspec)
            else:
                print ("unsupported command!: %s"%command)
                quit()
            print (finish_str(command, test["prog"], result))
            if (not result):
                test_failed = True
        results.append({"prog":test["prog"], "command":command, "result":result})
    return results

def compile(dptc, prog, harness, build):
    if (os.path.isdir(build) or os.path.isfile(build)):
        print ("deleting previous build directory...")
        shutil.rmtree(build)
    cmd = [dptc, prog, harness, build]
    print ("compile command: %s"%(" ".join(cmd)))
    ret = subprocess.run(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)   
    return (ret.returncode == 0)

def assemble(build):
    """ Compile P4 to the tofino """
    # 1. run make
    mk_cmd = "cd %s; make build"%build
    print ("assemble command: %s"%mk_cmd)
    ret = subprocess.run(mk_cmd, shell=True, stdout = subprocess.PIPE, stderr = subprocess.PIPE)   
    success = check_manifest(build)
    return success

def execute(build, test_spec):
    mk_cmd = "cd %s; make test %s"%(build, str(Path(test_spec).absolute()))
    print ("execute command: %s"%mk_cmd)
    ret = subprocess.run(mk_cmd, shell=True, stdout = subprocess.PIPE, stderr = subprocess.PIPE)   
    success = check_manifest(build)
    return success

def summarize(results):
    print ("**** summary ****")
    num_tests = len(results)
    num_passed = len([r for r in results if r["result"]])
    num_failed = len([r for r in results if not r["result"]])
    print (blue("%s/%s tests passed"%(num_passed, num_tests)))
    if (num_failed > 0):
        print (red("%s/%s tests failed"%(num_failed, num_tests)))
        print (red("**** failing tests ****"))
        for result in results:
            if (not result["result"]):
                print (cmd_str(result["command"], result["prog"]))
    else:
        print (blue("**** all tests passed ****"))
    return

def print_summary(test, result):
    if (test["type"] == "compile"):
        return "compile %s + %s to P4"%(test["prog"], test["harness"])
    elif (test["type"] == "assemble"):
        return "assemble %s to Tofino"%(test["build"])
    elif (test["type"] == "execute"):
        return "execute %s + %s on Tofino model"%(test["build"], test["testspec"])

def cleanup(builds_dir, results):
    num_tests = len(results)
    num_passed = len([r for r in results if r["result"]])
    if (num_tests == num_passed):
        shutil.rmtree(builds_dir)
    return

# find first file with this name on path from here up to root. 
def find_file(fn):
    abs_fn = None
    search_dir = os.path.dirname(os.path.realpath(__file__))
    potential_fn = search_dir+"/"+fn
    if (os.path.exists(potential_fn)):
        abs_fn = potential_fn
    while (abs_fn == None):
        search_dir = str(Path(search_dir).parent.absolute())
        potential_fn = search_dir+"/"+fn
        if (os.path.exists(potential_fn)):
            abs_fn = potential_fn
        elif (search_dir == "/"):
            break
    if (abs_fn == None):
        raise FileNotFoundError(errno.ENOENT, "Could not find file in any parent directory", fn)
    return abs_fn

def check_manifest(build):
    # check the manifest from P4 -> Tofino compilation 
    # to see if it succeeded.
    tofino_manifest = "%s/lucid/manifest.json"%build
    manifest = json.load(open(tofino_manifest, "r"))
    return manifest["compilation_succeeded"]


# printing helpers
def pfail(str):
    print (bcolors.FAIL +"[FAIL] "+ bcolors.ENDC + str)
def ppass(str):
    print (bcolors.OKCYAN +"[OK] "+ bcolors.ENDC + str)

def blue(st): return bcolors.OKCYAN + st + bcolors.ENDC
def red(st): return bcolors.FAIL + st + bcolors.ENDC
def emph(st): return bcolors.HEADER + st + bcolors.ENDC

class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


if __name__ == '__main__':
    main()
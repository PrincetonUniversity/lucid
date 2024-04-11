#!/usr/bin/env python3

# depreciated. There is no more old tofinocore. So to compare resource 
# usage, we have to pull the old tofinocore checkpoint from the git.

import os, sys, errno, shutil, subprocess, re, json, itertools
from sys import path
from pathlib import Path
from collections import namedtuple
import compilertests


# testprog = namedtuple('testprog', ['src', 'build_optimized', 'build_baseline'])

def src_to_build(build_root, src, opt_type):
    """
    Take a filename like "examples/tofinoapps/reflector.dpt"
    and translate it into a build directory "$build_root/reflector/$opt_type"
    """
    src_dir, src_file = os.path.split(src)
    src_name = os.path.splitext(src_file)[0]
    opt_build_dir = os.path.join(build_root, src_name, opt_type)
    return opt_build_dir

def main():
    # print("benchmarking new tofinocore optimizations")
    dptc = compilertests.find_file("dptc")
    repo_root = dir_of(dptc)
    # print("dptc: %s"%dptc)
    # print("base repo dir: %s"%repo_root)
    builds_root = "tofinocore_benchmark"
    os.makedirs(builds_root, exist_ok=True)
    # start with all the tofino test programs
    srcs = [
        "%s/examples/tofino_apps/src/egress_reflector.dpt"%repo_root,
        "%s/examples/tofino_apps/src/complex_memops.dpt"%repo_root,
        "%s/examples/tofino_apps/src/multi_events.dpt"%repo_root,
        "%s/examples/tofino_apps/src/r_after_w.dpt"%repo_root,
        "%s/examples/tofino_apps/src/reflector.dpt"%repo_root,
        "%s/examples/tofino_apps/src/identity_hash.dpt"%repo_root,
        "%s/examples/tofino_apps/src/control_reflector.dpt"%repo_root,
        "%s/examples/tofino_apps/src/simple_cuckoo_firewall.dpt"%repo_root,
        "%s/examples/tofino_apps/src/learner.dpt"%repo_root
    ]
    results = {}
    for src in srcs:
        res = benchmark_src(dptc, builds_root, src)
        results[os.path.basename(src)] = res
    for src, metrics in results.items():
        print ("results for %s"%src)
        for metric, vals in metrics.items():
            print ("  %s: %s"%(metric, vals))

def benchmark_src(dptc, builds_root, src, rebuild=False):
    # just print the filename that we are benchmarking, not the full path
    print ("benchmarking %s"%os.path.basename(src))
    results = {}
    for build_ty, build_flag in [("baseline", False), ("opt", True)]:
        build = src_to_build(builds_root, src, build_ty)
        os.makedirs(build, exist_ok=True)
        if ((not rebuild) and check_manifest(build)):
            pass
            # print ("manifest already exists for %s"%src)
        else:
            result = compile(dptc, src, build, build_flag)
            if (result == False):
                print ("compile failed for %s"%os.path.basename(src))
                return
            result = assemble(build)
            if (result == False):
                print ("assemble failed for %s"%os.path.basename(src))
                return
        res = analyze(build)
        results[build_ty] = res
    # print ("results: %s"%results)
    # reformat results so that outer dict is keyed by metric
    # and inner dict is keyed by build type
    metric_keys = results["baseline"].keys()
    results = {k: {build_ty: v[k] for build_ty, v in results.items()} for k in metric_keys}
    return results

def compile(dptc, prog, build, use_new_tofino):
    if (os.path.isdir(build) or os.path.isfile(build)):
        shutil.rmtree(build)
    cmd = []
    if (use_new_tofino):
        cmd = [dptc, prog, "-o", build]
    else: 
        cmd = [dptc, prog, "-o", build]        
    # print ("compile command: %s"%(" ".join(cmd)))
    ret = subprocess.run(cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE)   
    return (ret.returncode == 0)

def assemble(build):
    """ Compile P4 to the tofino """
    # 1. run make
    mk_cmd = "cd %s; make build"%build
    # print ("assemble command: %s"%mk_cmd)
    ret = subprocess.run(mk_cmd, shell=True, stdout = subprocess.PIPE, stderr = subprocess.PIPE)   
    success = check_manifest(build)
    return success

def analyze(build):
    """ Analyze resource usage of a build """
    tofino_bin_dir = "%s/lucid_tofino/pipe"%build
    resources_json_fn = "%s/logs/resources.json"%tofino_bin_dir
    if (not os.path.exists(resources_json_fn)):
        print ("resource usage json not found: %s"%resources_json_fn)
        return
    resources = json.load(open(resources_json_fn, "r"))["resources"]
    return {
        "n_stages": measure_nstages(resources)
    }

# measurement functions
def measure_nstages(resources):
    return len(resources["mau"]["mau_stages"])



def check_manifest(build):
    # check the manifest from P4 -> Tofino compilation 
    # to see if it succeeded.
    tofino_manifest = "%s/lucid_tofino/manifest.json"%build
    # if the manifest doesn't exist, compilation failed. We should catch 
    # the exception and return False
    if (not os.path.exists(tofino_manifest)):
        return False    
    manifest = json.load(open(tofino_manifest, "r"))
    return manifest["compilation_succeeded"]

def dir_of(dptc_fn): 
    return os.path.dirname(dptc_fn)

if __name__ == '__main__':
    main()
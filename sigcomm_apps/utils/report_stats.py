#!/usr/bin/env python3
# report loc and number of pipeline stages for built programs.
import sys, subprocess, json, glob, os

def main():
    app = sys.argv[1]
    measure_app(app)
    return

def measure_app(app):
    print ("lucid program:              %s"%lucid_src_of_app(app))
    print ("lucid program loc:          %s"%get_cloc(lucid_src_of_app(app)))
    print ("p4-tofino program loc:      %s"%get_cloc(p4t_src_of_app(app)))
    print ("number of tofino stages:    %s"%get_num_stages(p4t_log_dir_of_app(app)))
    return 

def get_num_stages(log_dir):
    try:
        resources = json.load(open(log_dir + "resources.json", "r"))
        return str(len(resources["resources"]["mau"]["mau_stages"]))
    except FileNotFoundError:
        return "[tofino build not found]"

def get_cloc(src_fn): 
    cloc_cmd = """ cat {0} | grep -v "^ *$" | grep -v "^ *//" | grep -v "^ */\*.*\*/" | wc -l""".format(src_fn)
    res = subprocess.run(cloc_cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if ((int(res.stdout)) == 0):
        return "[p4 code not found]"
    else:
        return str(int(res.stdout))

def p4t_log_dir_of_app(app): 
    return app + "/p4/lucid/pipe/logs/"

def lucid_src_of_app(app): 
    return glob.glob(app+"/*.dpt")[0]

def p4t_src_of_app(app):
    return app + "/p4/lucid.p4"

if __name__ == '__main__':
    main()
#!/usr/bin/env python3
import os
import re
import sys

rules = [
    (r'packet event (\w+)\s*\((.*?)\);', r'event \1(\2);'),
    (r'exit event (\w+)\s*\((.*?)\);', r'event \1(\2) {skip;}'),
    (r'group (\w+)\s*=', r'const group \1 ='),
    (r'mgenerate Event\.(s|m)mlocate\((.+),\s*(\w+)\);', r'generate_ports (\3, \2);'),
    (r'generate Event\.(s|m)slocate\((.+),\s*(\w+)\);', r'generate_port ((int<9>)\3, \2);'),
    # Special cases
    # dnsguard
    (r'event dnspkt_out\s*\(int<1> packet_allowed',
     r'event dnspkt_out (int<1> packet_allowed, int<31> padding'),
    (r'dnspkt_out\(TRUE', r'dnspkt_out(TRUE, 0'),
    # chain_stateful_firewall
    (r'const int (head|tail|succ)', r'const int<9> \1'),
    (r'int self', r'int<9> self'),
    # Undo the above for rip
    (r'const int<9> self', r'const int self'),
    # countmin_historical + starflow
    (r'memop read', r'memop fst'),
    (r'Array.update\((.+), read, (.+)\);', r'Array.update(\1, fst, \2);'),

    # Add more rules as needed
]

def patch_file(file_path):
    print(f"Patching file: {file_path}")

    with open(file_path, 'r') as file:
        content = file.read()

    for pattern, replacement in rules:
        content = re.sub(pattern, replacement, content)
    patched_file_path = file_path.replace('.dpt', '.patched.dpt')
    with open(patched_file_path, 'w') as file:
        file.write(content)

def patch_all():
    # Get the current directory
    current_dir = os.getcwd()

    # Traverse files in the current directory only
    for file in os.listdir(current_dir):
        if file.endswith('.dpt') and not file.endswith('.patched.dpt'):  # Adjust only non-patched .dpt files
            file_path = os.path.join(current_dir, file)
            patch_file(file_path)

if len(sys.argv) < 2:
    print("Usage: patch_script.py <file.dpt> | all")
    sys.exit(1)

arg = sys.argv[1]
if arg == "all":
    patch_all()
else:
    patch_file(arg)

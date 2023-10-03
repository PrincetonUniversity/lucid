#!/usr/bin/env python3
import os
import re
import sys

rules = [
    (r'packet event (\w+)\s*\((.*?)\);', r'event \1(\2);'),
    (r'exit event (\w+)\s*\((.*?)\);', r'event \1(\2){skip;}')
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
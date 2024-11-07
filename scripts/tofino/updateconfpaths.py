#!/usr/bin/env python3
# update configuration paths in a tofino conf file
import json
import os
import sys
import re

def update_paths_in_config(config_path):
    # Read the JSON configuration file
    with open(config_path, 'r') as f:
        config = json.load(f)

    # Get the directory of the config file
    config_dir = os.path.dirname(os.path.abspath(config_path))

    # Define the pattern to match paths containing "/build/lucid_tofino"
    pattern = re.compile(r'.*/build/lucid_tofino')

    # Function to update paths in the JSON structure
    def update_paths(obj):
        if isinstance(obj, dict):
            for key, value in obj.items():
                if isinstance(value, str) and pattern.match(value):
                    # Replace the part before "/build/lucid_tofino" with the new path
                    obj[key] = pattern.sub(config_dir, value)
                else:
                    update_paths(value)
        elif isinstance(obj, list):
            for item in obj:
                update_paths(item)

    # Update paths in the configuration
    update_paths(config)

    # Write the updated configuration back to the file
    with open(config_path, 'w') as f:
        json.dump(config, f, indent=4)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: {} <config_file>".format(sys.argv[0]))
        sys.exit(1)

    config_path = sys.argv[1]

    if not os.path.exists(config_path):
        print(f"Configuration file not found: {config_path}")
        sys.exit(1)

    # Update paths in the configuration file
    update_paths_in_config(config_path)
    print(f"Updated paths in configuration file: {config_path}")
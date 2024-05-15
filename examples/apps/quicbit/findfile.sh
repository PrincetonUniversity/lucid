#!/bin/bash

# Check if an argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi

filename="$1"
current_dir="$(pwd)"

while true; do
    # Check if the file exists in the current directory
    if [ -e "${current_dir}/${filename}" ]; then
        # Return the absolute path to the file
        echo "${current_dir}/${filename}"
        break
    fi

    # Check if the current directory is root
    if [ "${current_dir}" = "/" ]; then
        # File not found, exit with an error message
        # echo "[findfile.sh warning] File '${filename}' not found" >&2
        exit 2
    fi

    # Move up to the parent directory
    current_dir="$(dirname "${current_dir}")"
done

exit 0
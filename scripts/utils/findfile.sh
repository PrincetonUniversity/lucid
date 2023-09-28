#!/bin/bash

# Define the function to find a file
find_file() {
    local filename="$1"
    local current_dir="$(pwd)"

    while true; do
        # Check if the file exists in the current directory
        if [ -e "${current_dir}/${filename}" ]; then
            echo "${current_dir}/${filename}"
            return 0
        elif [ "${current_dir}" = "/" ]; then
            echo "File '${filename}' not found"
            return 2
        fi
        current_dir="$(dirname "${current_dir}")"
    done
}

# find_file() {
#     local filename="$1"
#     local current_dir="$(pwd)"
#     while [ "${current_dir}" != "/" ]; do
#         local result="$(find "${current_dir}" -name "${filename}" -print -quit 2>/dev/null)"
#         [ -n "${result}" ] && echo "${result}" && return 0
#         current_dir="$(dirname "${current_dir}")"
#     done
#     echo "File '${filename}' not found"
#     return 2
# }

# Check if an argument is provided
if [ $# -ne 1 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi
find_file "$1"
exit $?
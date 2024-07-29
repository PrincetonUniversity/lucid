#!/bin/bash
# Usage: patch_lucidpy.sh <python_file> <lucid.py_file>
# This script copies a custom python file that contains a user-defined 
# lucid_ctl function into the given lucid.py file. 
# The custom python file is placed immediately before the main function def
# The script will replace the existing custom block if it was 
# added with this script. 

# check args
if [ "$#" -ne 2 ]; then
    echo "Usage: patch_lucidpy.sh <python_file> <lucid.py_file>"
    exit 1
fi

# make sure both files exist
if [ ! -f "$1" ]; then
    echo "Error: $1 does not exist"
    exit 1
fi
if [ ! -f "$2" ]; then
    echo "Error: $2 does not exist"
    exit 1
fi
# make sure the script has "def main():" in it
if ! grep -q "def main():" "$2"; then
    echo "Error: lucid.py file does not contain 'def main():'"
    exit 1
fi

# Check if the python file has already been copied
if grep -q "#begin inserted user-function" "$2"; then
    # If the python file has been copied, delete the copied part
    sed -i '/#begin inserted user-function/,/#end inserted user-function/d' "$2"
fi

# Tell the user what we are doing
echo "***Patching $2 by inserting the contents of $1 before the main function"
echo "***Contents of $1:"
cat "$1"
echo ""

# Split the lucid.py file into two parts: before "def main():", and including and after
awk '/def main\(\):/{exit} {print}' "$2" > before_main.py
awk 'BEGIN{p=0}/def main\(\):/{p=1}p' "$2" > after_main.py

# make sure the split worked
if [ ! -f before_main.py ]; then
    echo "Error: failed to split lucid.py file"
    exit 1
fi
if [ ! -f after_main.py ]; then
    echo "Error: failed to split lucid.py file"
    exit 1
fi

# Add the marker comments to the python file
echo "#begin inserted user-function" > user_function.py
cat "$1" >> user_function.py
echo "" >> user_function.py
echo "#end inserted user-function" >> user_function.py

# Concatenate the python file and the two parts of the lucid.py file
cat before_main.py user_function.py after_main.py > $2

# Clean up the temporary files
rm before_main.py after_main.py user_function.py
echo "***done adding the contents of $1 to $2"
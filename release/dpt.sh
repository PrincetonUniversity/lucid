#!/bin/bash

script_dir=$(dirname $0)

macos_dir="$script_dir/macos/lucid"
linux_dir="$script_dir/linux/lucid"

# set bin_dir to the correct directory based on the OS
if [[ "$OSTYPE" == "darwin"* ]]; then
  bin_dir=$macos_dir
  if [ ! -f "$bin_dir/dpt" ]; then
    echo "MacOS binary not found in $macos_dir"
    echo "You may need to run the 'unpack.sh' script in the release directory"
    exit 1
  fi
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [ ! -f "$linux_dir/dpt" ]; then
    echo "Linux binary not found in $linux_dir"
    echo "You may need to run the 'unpack.sh' script in the release directory"
    exit 1
  fi
  bin_dir=$linux_dir
else
  echo "Unsupported OS -- you should build the project from source"
  exit 1
fi

# run dpt in the appropriate directory
$bin_dir/dpt $@



# case "$1" in
#   compile)
#     shift # Remove the first argument and pass the rest to the compiler
#     exec "$APPDIR/usr/bin/dptc" "$@"
#     ;;
#   interpret)
#     shift # Remove the first argument and pass the rest to the interpreter
#     exec "$APPDIR/usr/bin/dpt" "$@"
#     ;;
#   *)
#     echo "Usage: $0 {compile|interpret} args"
#     exit 1
#     ;;
# esac
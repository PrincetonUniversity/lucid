#!/bin/bash
# build one project
if [ "$#" -ne 2 ]; then
  echo "Invalid number of arguments. Usage: script_name <arg1> <arg2>"
  exit 1
fi

filename=$(basename "$2")
build_dir="${filename%.*}_build"

if [ "$1" = "lucid" ]; then
  cmd="../../../dptc $2 -o $build_dir"
  output_file="${filename%.*}_lucid_time.txt"
  echo "$cmd"
  { time eval "$cmd"; } 2> "$output_file"
  
elif [ "$1" = "p4" ]; then
  cd "$build_dir" || exit
  output_file="../${filename%.*}_p4_time.txt"
  { time make; } 2> "$output_file"
  
else
  echo "Invalid argument: $1"
fi
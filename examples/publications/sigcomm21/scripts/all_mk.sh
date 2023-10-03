#!/bin/bash
# build all projects in directory
if [ "$#" -ne 1 ]; then
  echo "Invalid number of arguments. Usage: mk_all.sh <arg>"
  exit 1
fi

arg="$1"
if [ "$arg" != "lucid" ] && [ "$arg" != "p4" ]; then
  echo "Invalid argument: $arg. Must be either 'lucid' or 'p4'."
  exit 1
fi

for file in *.dpt; do
  if [ -f "$file" ]; then
    ./mk.sh "$arg" "$file"
  fi
done
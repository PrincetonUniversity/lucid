#!/bin/bash

# make sure this is a macos machine
if [ "$(uname)" != "Darwin" ]; then
  echo "The MacOS release should be built on a MacOS system."
  exit 1
fi
# make sure we have dylibbundler
if ! command -v dylibbundler &> /dev/null
then
    echo "dylibbundler could not be found. Please install it using 'brew install dylibbundler'"
    exit
fi

release_dir=./macos/lucid
lib_dir=$release_dir/libs

rm -rf $release_dir
mkdir -p $lib_dir

cd ..
make
cd - 

cp ../dpt $release_dir/

# run dylibbundler to bundle the dynamic libraries (mainly z3)
dylibbundler -od -b -x $release_dir/dpt -d $lib_dir -p @executable_path/libs/
# print release information
echo "======================"
echo "MacOS binary package built in $release_dir. Please distribute this entire folder and run dpt inside of it."
echo "======================"    

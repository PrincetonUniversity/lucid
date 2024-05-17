#!/bin/bash

# unpack the release tarballs in this script's directory

my_dir="$(dirname "$0")"

# check if each tarball exists and unpack it
if [ ! -f $my_dir/macos.tar.gz ]; then
  echo "macos.tar.gz not found -- skipping"
else 
  tar -xvf $my_dir/macos.tar.gz -C $my_dir
fi

if [ ! -f $my_dir/linux.tar.gz ]; then
  echo "linux.tar.gz not found -- skipping"
else
  tar -xvf $my_dir/linux.tar.gz -C $my_dir
fi
#!/bin/bash

# make sure this is an ubuntu or debian machine
if [ -f /etc/os-release ]; then
  . /etc/os-release
  if [ "$ID" != "ubuntu" ] && [ "$ID" != "debian" ]; then
    echo "The linux release should be built on an Ubuntu or Debian system."
    exit 1
  fi
else
  echo "The linux release should be built on an Ubuntu or Debian system."
  exit 1
fi

# make sure this is run from the repo root
if [ ! -d ".git" ]
then
  echo "This script must be run from the root of the repository."
  exit 1
fi

release_base=./release
os_base=linux
arch=$(uname -m)

release_dir=$release_base/$os_base/lucid
lib_dir=$release_dir/lib

rm -rf $release_dir
mkdir -p $lib_dir

# 1. build the binary locally (in the parent directory)
make

# copy binary
cp dpt "$release_dir"/dpt

# Run ldd on the binary and parse output
deps=$(ldd "$release_dir"/dpt | awk '/=>/ {print $3} !/=>/ {if ($1 ~ /^\//) print $1}')

# Exclude system libs that come with linux
exclude_libs="libc.so.6 libstdc++.so.6 libgcc_s.so.1 ld-linux-x86-64.so.2 libpthread.so.0 libdl.so.2 libm.so.6"
for lib in $deps; do
  base_lib=$(basename "$lib")
  if [[ " $exclude_libs " =~ " $base_lib " ]]; then
    echo "Skipping $lib"
    continue
  fi
  if [ -f "$lib" ]; then
    echo "Copying $lib to $lib_dir"
    cp "$lib" "$lib_dir"
  else
    echo "Library $lib not found."
  fi
done

echo "patching binary dynamic lib paths"
chmod +w "$release_dir"/dpt
patchelf --set-rpath '$ORIGIN/lib' "$release_dir"/dpt

# package os release in a tarball inside of the release dir
echo "Packaging release"
# put the lucid directory into a tar named lucid.$os_base_.tar.gz
cd $release_dir/..
echo `pwd`
tar -zcvf lucid."$os_base"."$arch".tar.gz lucid
# remove the release directory
cd -
rm -rf $release_dir


# # package os release in a tarball inside of the release dir
# echo "Packaging release"
# cd $release_base
# tar -zcvf "$os_base".tar.gz $os_base
# rm -rf $os_base

echo "done building linux release"

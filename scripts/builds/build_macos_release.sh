#!/bin/bash
# build a release binary on macos.

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

# make sure this is run from the repo root
if [ ! -d ".git" ]
then
  echo "This script must be run from the root of the repository."
  exit 1
fi

release_base=./release
os_base=macos
arch=$(uname -m)

release_dir=$release_base/$os_base/lucid
lib_dir=$release_dir/lib

rm -rf $release_dir
mkdir -p $lib_dir

make

cp dpt $release_dir/

# run dylibbundler to bundle the dynamic libraries (mainly z3)
echo "patching binary dynamic lib paths"
chmod +w $release_dir/dpt
dylibbundler -od -b -x $release_dir/dpt -d $lib_dir -p @executable_path/lib/

echo "# OSX permissions (run to give trust to dpt and libraries)" > $release_dir/osxPermissions.sh
echo 'xattr -r -d com.apple.quarantine "$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"' >> $release_dir/osxPermissions.sh
chmod a+x $release_dir/osxPermissions.sh
# package os release in a tarball inside of the release dir
echo "Packaging release"
# put the lucid directory into a tar named lucid.$os_base_.tar.gz
cd $release_dir/..
echo `pwd`
tar -zcvf lucid."$os_base"."$arch".tar.gz lucid
# remove the release directory
cd -
rm -rf $release_dir

echo "done building macos release"


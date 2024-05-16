#!/bin/bash
# build release versions of the interpreter and compiler 
# first argument should be the target: macos or linux

if [ "$1" == "macos" ]; then
    echo "Building for MacOS"
    # 1. build the binary locally (in the parent directory)
    cd ..
    make
    cd - 
    # 2. make the osx release dir if it does not exist
    mkdir -p macos/lucid
    # 3. copy the binary to the release dir
    cp ../dpt macos/lucid/
    # 4. run dylibbundler to bundle the dylibs
    dylibbundler -od -b -x macos/lucid/dpt -d macos/lucid/libs/ -p @executable_path/libs/
    # 5. print release information
    echo "======================"
    echo "MacOS binary package built in ./macos/lucid. Please distribute this entire folder."
    echo "======================"    
elif [ "$1" == "linux" ]; then
    echo "Building for Linux"
    mkdir -p linux/lucid/usr/bin
    # 1. build the binary locally (in the parent directory)
    cd ..
    make
    cd -
    # 2. copy the binary to the release dir
    cp ../dpt linux/lucid/usr/bin/
    # 3. copy "lucid" script to the release dir
    cp lucid linux/lucid/
    # 4. run linuxdeploy to bundle the libs
    # ./linuxdeploy-x86_64.AppImage --appdir linux/lucid --output appimage
    # 5. print release information
    echo "======================"
    echo "Linux AppImage binary built in ./linux/lucid. Please distribute this entire folder."
    echo "======================"    
else
    echo "Invalid target: $1"
fi
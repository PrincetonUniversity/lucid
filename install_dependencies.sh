#!/bin/bash
# check for and install Lucid dependencies
# this should work on any system with opam installed, 
# or on Ubuntu and Debian if opam is not installed

# Check for opam and install if not found
if ! command -v opam &> /dev/null
then
    # opam install -- we can do this on Ubuntu and Debian, 
    # for other OSes you're on your own.
    # Update system packages
    sudo apt-get update -y
    # Check if the script is running on a supported version of Linux
    if ! grep -qE 'Ubuntu|Debian' /etc/os-release; then
        echo "Opam is not installed and this script can only auto-install it for Ubuntu and Debian. Please install opam manually and re-run this script."
        exit 1
    fi
    sudo apt-get install opam -y
fi

# Initialize opam, if not already initialized
if ! opam var root &> /dev/null
then
    opam init -y --auto-setup
fi

if ! command -v opam &> /dev/null
then
    echo "Opam not working, even after install and init attempts. Exiting."
    exit 1
fi


# check for opam depext and install if not found
if ! opam list --installed depext &> /dev/null
then
    opam install depext -y
    if ! opam list --installed depext &> /dev/null
    then
        echo "Failed to find or install depext. Exiting."
        echo "If you are on macos, the install may have failed because you need homebrew to use depext"
        exit 1
    fi
fi

# Install lucid dependencies with opam
opam switch create 4.12.0
opam switch 4.12.0
opam depext -y .
opam install -y --deps-only .

echo "dependencies installed. You should be able to build lucid with 'make' now."
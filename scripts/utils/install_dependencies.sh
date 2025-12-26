#!/bin/bash
# check for and install Lucid dependencies
# this should work on any system with opam installed, 
# or on Ubuntu and Debian if opam is not installed

# Check for opam and install if not found
if ! command -v opam &> /dev/null
then
    # opam install -- we can do this on Ubuntu and Debian, 
    # for other OSes you're on your own.
    if ! grep -qE 'Ubuntu|Debian' /etc/os-release; then
        echo "Opam is not installed and this script can only auto-install it for Ubuntu and Debian. Please install opam manually and re-run this script."
        exit 1
    fi
    # Update system packages
    sudo apt update
    sudo apt install -y build-essential
    sudo apt install -y opam
fi

# check that the opam version is greater than 2.1.0
versions=$(printf '2.1.0\n'$(opam --version))
sorted_versions=$(echo "$versions" | sort -V)
first_version=$(echo "$sorted_versions" | head -n 1)
if [ "$first_version" != "2.1.0" ]; then
    echo "Opam version is too old. Please update opam to >= 2.1.0 and re-run this script."
    exit 1
fi

# Initialize opam, if not already initialized
if ! opam var root &> /dev/null
then
    opam init -y --auto-setup
fi
# eval $(opam env)

# Install lucid dependencies with opam
# switch to OCaml 4.12.0
opam switch create 4.12.0 
opam switch 4.12.0
# install dependencies
if [ ! -f "dpt.opam" ]; then
    echo "Error: dpt.opam not found in the current directory."
    echo "To complete installation, run the following command"
    echo "from the root directory of the lucid repo, where dpt.opam is located:"
    echo "opam install -y --confirm-level=unsafe-yes --deps-only ."
    exit 1
fi
opam install -y --confirm-level=unsafe-yes --deps-only .
echo "All Lucid dependencies installed."
echo "Run eval \$(opam env) to update your shell env before calling make."

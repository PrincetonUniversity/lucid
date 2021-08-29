
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo ERROR: This script is supposed to be sourced, not executed
    exit 1
fi


export LDE=`pwd`
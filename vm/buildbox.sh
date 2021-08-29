BOX="lucid.box"
BUILD_DIR="lucidbox"

function abs_path() {
    echo "$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"    
}
function my_path() {
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
    echo $SCRIPT_DIR
}

# configurable build parameters
NUM_CPUS=6
SDE=$(abs_path bf-sde-9.5.0.tgz)

if [ -e $BOX ]
then 
    echo "$BOX already present"
else
    if [ -e $BUILD_DIR/$BOX ]
    then 
        echo "$BOX already built"
        mv $BUILD_DIR/$BOX ./
    else
        echo "Building new $BOX"
        cd "$(my_path)/$BUILD_DIR"
        if [ -e $SDE ]
        then 
            ./build.sh --sde="$SDE" --cpus="$NUM_CPUS"
        else
            ./build.sh --cpus="$NUM_CPUS"
        fi
        cd -
        mv $BUILD_DIR/$BOX ./
    fi
fi
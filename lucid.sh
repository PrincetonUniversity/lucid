# Dockerize lucid environment. 
# This script compiles and executes Lucid
# in a docker image, on files / folders shared from your machine.
# requirements: docker
# usage: 
# ./lucid.sh build -- build lucid binaries inside of a docker image and place them here.
# ./lucid.sh interp <args> -- run lucid interpreter inside of the docker image.
# ./lucid.sh compile <args> -- run lucid compiler inside the docker image.


to_parent_dir()
{
    cd $1 && cd ../ && pwd
}
check_lucid_dir()
{
    if [ -f "$1/dpt.opam" ]; 
    then
        continue;
    else
        exit 1;
    fi    
}
is_file_or_dir()
{
    retval=1
    if   [ -d "${var}" ] || [ -f "${var}" ]
    then
        retval=0
    fi
    return $retval
}
to_absolute_path() 
{
    var=$1
    if   [ -d "${var}" ]
    then 
        ABSOLUTE_PATH=$(cd $(dirname ${var}); pwd)
        echo $ABSOLUTE_PATH
    elif [ -f "${var}" ]
    then 
        ABSOLUTE_PATH=$(cd $(dirname ${var}); pwd)
        BASE=$(basename ${var})
        echo $ABSOLUTE_PATH/$BASE
    else 
        echo $var
    fi
}


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
LUCID_DIR="$SCRIPT_DIR"
# LUCID_DIR=$(to_parent_dir "$SCRIPT_DIR")
DOCKER_IMAGE="jsonch/lucid:ocamlbase"

check_lucid_dir $LUCID_DIR

BASE_CMD="docker run --rm -it -v $LUCID_DIR:/home/opam/lucid $DOCKER_IMAGE /bin/bash -c \"cd /home/opam/lucid; make"
PARSE_ARGS=0
ARGS=""
case $1 in
    build)
        echo "building lucid"
        CMD="$BASE_CMD\""
        ;;
    interpret)
        echo "running lucid interpreter"
        CMD="$BASE_CMD && ./dpt"
        PARSE_ARGS=1
        ;;
    interp)
        echo "running lucid interpreter"
        CMD="$BASE_CMD && ./dpt"
        PARSE_ARGS=1
        ;;
    compile)
        echo "running lucid compiler"
        CMD="$BASE_CMD && ./dptc"
        PARSE_ARGS=1
        ;;
    *)
        echo "usage: ./lucid.sh <interpret | compile> <arguments to lucid interpreter or compiler>"
        exit 1;
        ;;
esac

if (( PARSE_ARGS==1 ))
then
    for var in "$@"
    do
        if is_file_or_dir $var
        then 
            abs_fn=$(to_absolute_path "$var")
            remote_fn="/home/opam/inputs/$(basename ${var})"
            CMD="$CMD -v $abs_fn:$remote_fn"
            ARGS="$ARGS $remote_fn"
        else
            ARGS="$ARGS $var"        
        fi
        # echo $?
    done
    CMD="$CMD\""
fi

echo "command: $CMD"
eval "$CMD"
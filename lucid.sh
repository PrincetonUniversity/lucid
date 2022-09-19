#### Dockerize lucid build environment ####
# This script compiles and executes Lucid in a docker image, 
# then prints or copies the output back to your machine. 

# requirements: docker

# usage: 
# ./lucid.sh interp <prog.dpt> -- run lucid interpreter inside of the docker image.
# ./lucid.sh compile <prog.dpt> -- run lucid compiler inside the docker image.
# ./lucid.sh rebuild -- rebuild docker image with local version of source
# ./lucid.sh pull -- pull most recently published version of docker image
# ./lucid.sh rebuild_and_push -- rebuild docker image and publish (admin only)


to_parent_dir()
{
    cd $1 && cd ../ && pwd
}
is_file_or_dir()
{
    retval=1
    if   [ -d "${1}" ] || [ -f "${1}" ]
    then
        retval=0
    fi
    return $retval
}

to_absolute_path() 
{
    ABSOLUTE_PATH=$(cd $(dirname ${1}); pwd)
    BASE=$(basename ${1})
    echo $ABSOLUTE_PATH/$BASE    
}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
LUCID_DIR="$SCRIPT_DIR"
DOCKER_IMAGE="jsonch/lucid:lucid"

DOCKER_CMD="docker run --rm -it"
CMD=""
ARGS=""
BUILD_DIR=""    
IN_FN=""

interpret_cmd ()
{
    PROG=""
    SPEC=""
    COMPILER="./dpt"
    while [[ $# -gt 0 ]]; do
        case $1 in 
            # spec file, which may be inferred from program
            --spec)
                SPEC="$2"
                shift
                shift
                ;;            
            # input program. Should have .dpt name
            *.dpt)
                PROG="$1"
                shift
                ;;
            # everything else: if its a file or directory, 
            # mount it. If not, just pass as an argument.
            *)
                if is_file_or_dir $1
                then 
                    abs_fn=$(to_absolute_path "$1")
                    remote_fn="/app/inputs/$(basename ${1})"
                    DOCKER_CMD="$DOCKER_CMD -v $abs_fn:$remote_fn"
                    ARGS="$ARGS $remote_fn"
                else
                    ARGS="$ARGS $1"        
                fi
                shift
                ;;
        esac
    done

    # prepare prog: mount and add arg
    if [[ $PROG != "" ]]
    then 
        local=$(to_absolute_path "$PROG")
        remote="/app/inputs/$(basename ${PROG})"
        DOCKER_CMD="$DOCKER_CMD -v $local:$remote"
        ARGS="$ARGS $remote"
    else
        echo "ERROR: please provide a .dpt file as input."
    fi

    # prepare spec. If not present, 
    # infer based on program name.
    if [[ $SPEC == "" ]]
    then 
        SPEC="$(basename ${PROG} .dpt).json"
    fi
    # add spec if it exists.
    if   [ -f "$SPEC" ]
    then 
        local=$(to_absolute_path "$SPEC")
        remote="/app/inputs/$(basename ${SPEC})"
        DOCKER_CMD="$DOCKER_CMD -v $local:$remote"
        ARGS="$ARGS --spec $remote"
    fi

    CMD="$DOCKER_CMD $DOCKER_IMAGE /bin/sh -c \"$COMPILER$ARGS\""
    echo $CMD

}

compile_cmd () 
{
    PROG=""
    SPEC=""
    BUILD_DIR=""
    COMPILER="./dptc"
    while [[ $# -gt 0 ]]; do
        case $1 in 
            # spec file, which may be inferred from program
            --spec)
                SPEC="$2"
                shift
                shift
                ;;            
            # output directory, which may not exist
            -o)
                BUILD_DIR="$2"
                shift
                shift
                ;;
            # input program. Should have .dpt name
            *.dpt)
                PROG="$1"
                shift
                ;;
            # everything else: if its a file or directory, 
            # mount it. If not, just pass as an argument.
            *)
                if is_file_or_dir $1
                then 
                    # if its a folder, we share the parent directory. 
                    # if its a file, we share the file itself
                    abs_fn=$(to_absolute_path "$1")
                    remote_fn="/app/inputs/$(basename ${1})"
                    DOCKER_CMD="$DOCKER_CMD -v $abs_fn:$remote_fn"
                    ARGS="$ARGS $remote_fn"
                else
                    ARGS="$ARGS $1"        
                fi
                shift
                ;;
        esac
    done

    # prepare prog: mount and add arg
    if [[ $PROG != "" ]]
    then 
        local=$(to_absolute_path "$PROG")
        remote="/app/inputs/$(basename ${PROG})"
        DOCKER_CMD="$DOCKER_CMD -v $local:$remote"
        ARGS="$ARGS $remote"
    else
        echo "ERROR: please provide a .dpt file as input."
    fi

    # prepare spec. This is optional for the compiler.
    # so if there's no spec, we don't add one.
    if [[ $SPEC != "" ]]
    then 
        SPEC="$(basename ${PROG} .dpt).json"
        local=$(to_absolute_path "$SPEC")
        remote="/app/inputs/$(basename ${SPEC})"
        DOCKER_CMD="$DOCKER_CMD -v $local:$remote"
        ARGS="$ARGS $remote"
    fi

    # prepare output. 
    # 1. if no build is given, create a default.
    if [[ $BUILD_DIR == "" ]]
    then
        BUILD_DIR="$(basename ${PROG} .dpt)_build"
    fi
    # 2. if build dir doesn't exist, create it.
    if [ -f "${BUILD_DIR}" ]
    then
        echo "ERROR: build directory $BUILD_DIR is an already existing FILE."
        exit 1
    fi
    if [ ! -d "${BUILD_DIR}" ]
    then
        mkdir -p "$BUILD_DIR"
    fi
    # 3. get the absolute dir. 
    BUILD_DIR=$(to_absolute_path "$BUILD_DIR")
    # 4. build in a staging directory, then copy over all at once.
    BUILD_STAGE_DIR="/app/build"
    BUILD_FINAL_DIR="/app/final_build"
    DOCKER_CMD="$DOCKER_CMD -v $BUILD_DIR:$BUILD_FINAL_DIR"
    # 5. add args: -o /app/inputs/build
    ARGS="$ARGS -o $BUILD_STAGE_DIR"
    # add to end of command: move staging build to final.
    CMD="$DOCKER_CMD $DOCKER_IMAGE /bin/sh -c \"$COMPILER$ARGS; rm -rf $BUILD_FINAL_DIR/*; mv $BUILD_STAGE_DIR/* $BUILD_FINAL_DIR/\""
    echo $CMD
}

ensure_docker()
{
    if ! command -v docker -v &> /dev/null
    then
        echo "docker not found. Please install. Desktop version: https://www.docker.com/products/docker-desktop/"
        exit 1
    fi
}

check_lucid_dir()
{
    if [ -f "./dpt.opam" ]; 
    then
        continue
    else
        echo "error: you must run this command in the root of the lucid repo."
        exit 1
    fi    
}

# --- MAIN ---
ensure_docker

case $1 in
    # rebuild the docker image from local source, 
    # e.g., after a compiler update
    rebuild)
        shift 
        check_lucid_dir
        CMD="docker build -t $DOCKER_IMAGE ."
        ;;
    # rebuild and push to docker hub -- will only work for admins
    rebuild_and_push)
        shift 
        check_lucid_dir
        CMD="docker build -t $DOCKER_IMAGE .  && docker push $DOCKER_IMAGE"
        ;;        
    # get latest from docker hub
    pull)
        shift
        CMD="docker pull $DOCKER_IMAGE"
        ;;
    interp)
        shift
        CMD=$(interpret_cmd "$@")
        ;;
    interpret)
        shift
        CMD=$(interpret_cmd "$@")
        ;;
    compile)
        shift
        CMD=$(compile_cmd "$@")
        ;;
    *)
        echo "usage: ./lucid.sh <interpret | compile> <arguments to lucid interpreter or compiler>"
        exit 1;
        ;;
esac


echo "COMMAND:$CMD"
eval "$CMD"
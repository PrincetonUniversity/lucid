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


DOCKER_IMAGE="jsonch/lucid:lucid"
DOCKER_CMD="docker run --rm -it"
DOCKER_PWD="/app"

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

strip_whitespace()
{
    echo $1 | tr -d '[:space:]'
}
# get the first-level includes of a file
get_includes()
{
    DOCKER_UTIL="bin/dockerUtils includes"
    next=$1
    # >&2 echo "get_includes is processing: $next"
    # local and remote mount points -- you can still use next as the arg
    local=$(to_absolute_path $next)
    remote="$DOCKER_PWD/$next"
    # >&2 echo "get_includes local: $local"
    # >&2 echo "get_includes remote: $remote"   
    # GICMD="$DOCKER_CMD $(mount_file_str ${next}) /bin/sh -c \"" 
    GICMD="docker run --rm -it --mount type=bind,source=$local,target=$remote $DOCKER_IMAGE /bin/sh -c \"$DOCKER_UTIL $next\""
    # >&2 echo "GICMD:$GICMD"
    next_includes=$(eval "$GICMD")
    next_includes=$(strip_whitespace $next_includes)
    # `docker run --rm -it --mount type=bind,source=$local,target=$remote $DOCKER_IMAGE /bin/sh -c "bin/dockerUtils includes $next"`
    # >&2 echo "result: $next_includes"
    echo "$next_includes"
}

# get included files, recursively to any depth
get_all_includes()
{
    src=$1
    # >&2 echo "get_all_includes is processing: $src"
    src_includes_str=$(get_includes $src)
    IFS=':' read -ra incl_arr <<< "$src_includes_str"
    for dep_src in "${incl_arr[@]}"; do
        # >&2 echo "get_all_includes is processing dependency: $dep_src"
        # get dependencies of dependent
        recursive_includes_str=$(get_all_includes $dep_src)
        # if there are any 2nd-level dependencies, add them
        if [ -z "$recursive_includes_str" ]
        then
            src_includes_str="$src_includes_str"
        else
            src_includes_str="$src_includes_str:$recursive_includes_str"
        fi
        # >&2 echo "src_includes_str from $src-- $src_includes_str"
    done
    echo "$src_includes_str"
}

# get the main source file
get_main()
{
    DOCKER_PARSE="bin/dockerUtils main"
    # echo "docker run --rm -it $DOCKER_IMAGE /bin/sh -c \"$DOCKER_PARSE $@\""
    main=`docker run --rm -it $DOCKER_IMAGE /bin/sh -c "$DOCKER_PARSE $@"`
    main=$(strip_whitespace $main)
    echo "$main"    
}

# return a space-delimited list 
# of the source files used by the program 
get_all_sources()
{
    main=$(get_main $@)
    # 1. figure out name of main file.
    all_includes_str=$(get_all_includes $main)
    all_files_str="$main:$all_includes_str"
    IFS=':' read -ra all_files_arr <<< "$all_files_str"

    # unique elements of array 
    IFS=" " read -r -a all_files_arr <<< "$(echo "${all_files_arr[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' ')"
    echo "${all_files_arr[@]}"
}

# get the docker-local filename of a file passed as an argument to this script
docker_local_fn ()
{
    remote="/app/inputs/$(basename ${1})"
    echo "$remote"
}
# get the absolute local filename of a file passed as an argument to this script 
local_abs_fn () 
{
    local=$(to_absolute_path "$1")
    echo "$local"
}

mount_file_str () 
{
    echo "--mount type=bind,source=$(local_abs_fn ${1}),target=$(docker_local_fn ${1})"
}

interpret_cmd ()
{
    main=$(get_main $@)
    sources_str=$(get_all_sources $@)
    IFS=' ' read -ra sources <<< "$sources_str"
    >&2 echo "program: $main"
    >&2 echo "sources: $sources_str"
    SPEC_FOUND=0
    ARGS=""
    MOUNT=""
    COMPILER="./dpt"
    while [[ $# -gt 0 ]]; do
        # skip main 
        if [[ "$1" == "$main" ]]
        then
            # >&2 echo "skipping main arg"
            # replace with argument: docker local file
            ARGS="$ARGS $(docker_local_fn ${1})"
            # mount all of the sources to docker
            for src in "${sources[@]}"; do
                MOUNT="$MOUNT $(mount_file_str ${src})"
            done
            shift
        else
            case $1 in 
                --spec)
                    SPEC_FOUND=1
                    ARGS="$ARGS --spec $(docker_local_fn ${2})"
                    MOUNT="$MOUNT $(mount_file_str ${2})"
                    shift
                    shift
                    ;;  
                --symb)
                    ARGS="$ARGS --symb $(docker_local_fn ${2})"
                    MOUNT="$MOUNT $(mount_file_str ${2})"
                    shift
                    shift
                    ;;                                
                # everything else: just a flag. append to arg.
                *)
                    ARGS="$ARGS $1"
                    shift
                    ;;
            esac
        fi
    done
    # if there is no provided spec file, check for a <src>.json
    if [[ $SPEC_FOUND == 0 ]]
    then
        >&2 echo "no interp spec file provided"
        spec=${main%.dpt}.json
        >&2 echo "checking if spec exists: $spec"
        if [[ -f "$spec" ]]
        then
            >&2 echo "spec exists, adding args"
            ARGS="$ARGS --spec $(docker_local_fn ${spec})"
            MOUNT="$MOUNT $(mount_file_str ${spec})"
        fi
    fi
    # finally, prepare the command string. 
    CMD="$DOCKER_CMD $MOUNT $DOCKER_IMAGE /bin/sh -c \"$COMPILER$ARGS\""
    >&2 echo "running command: $CMD"
    eval $CMD
}


# build dir is global -- we want to print it out at the end as it might be derived.
BUILD_DIR=""
compile_cmd () 
{
    main=$(get_main $@)
    sources_str=$(get_all_sources $@)
    IFS=' ' read -ra sources <<< "$sources_str"
    SPEC_FOUND=0
    ARGS=""
    MOUNT=""
    COMPILER="./dptc"

    while [[ $# -gt 0 ]]; do
        if [[ "$1" == "$main" ]]
        then
            # replace main filename with docker local filename
            ARGS="$ARGS $(docker_local_fn ${1})"
            # mount all of the sources to docker (incl main)
            for src in "${sources[@]}"; do
                MOUNT="$MOUNT $(mount_file_str ${src})"
            done
            shift
        else
            case $1 in 
                --spec)
                    SPEC_FOUND=1
                    ARGS="$ARGS --spec $(docker_local_fn ${2})"
                    MOUNT="$MOUNT $(mount_file_str ${2})"
                    shift
                    shift
                    ;;            
                # output directory, process this at the end.
                -o)
                    BUILD_DIR="$2"
                    shift
                    shift
                    ;;
                # everything else: just a flag. append to arg.
                *)
                    ARGS="$ARGS $1"
                    shift
                    ;;
            esac
        fi
    done

    # if there is no provided spec file, check for a <src>.json
    if [[ $SPEC_FOUND == 0 ]]
    then
        >&2 echo "no interp spec file provided"
        spec=${main%.dpt}.json
        >&2 echo "checking if spec exists: $spec"
        if [[ -f "$spec" ]]
        then
            >&2 echo "spec exists, adding args"
            ARGS="$ARGS --spec $(docker_local_fn ${spec})"
            MOUNT="$MOUNT $(mount_file_str ${spec})"
        fi
    fi
    # build directory arg.
    # 1. if no build is given, create a default.
    if [[ $BUILD_DIR == "" ]]
    then
        BUILD_DIR=${main%.dpt}_build
    fi
    >&2 echo "build dir: $BUILD_DIR"
    # 2. if build dir doesn't exist locally, create it.
    if [ -f "${BUILD_DIR}" ]
    then
        echo "ERROR: build directory $BUILD_DIR is an already existing FILE."
        exit 1
    fi
    if [ ! -d "${BUILD_DIR}" ]
    then
        mkdir -p "$BUILD_DIR"
    fi
    # 3. get the absolute path to the build directory.
    BUILD_DIR=$(to_absolute_path "$BUILD_DIR")
    # 4. build in a staging directory, then copy over to where $BUILD_DIR is mounted
    BUILD_STAGE_DIR="/app/build"
    BUILD_FINAL_DIR="/app/final_build"
    # mount the final build dir
    MOUNT="$MOUNT -v $BUILD_DIR:$BUILD_FINAL_DIR"
    # 5. add staging dir as argument to compiler
    ARGS="$ARGS -o $BUILD_STAGE_DIR"
    # 6. after compiler finishes, copy staging build to final build.
    COPY_TO_FINAL="rm -rf $BUILD_FINAL_DIR/*; mv $BUILD_STAGE_DIR/* $BUILD_FINAL_DIR/"
    >&2 echo "build dir: $BUILD_DIR"
    CMD="$DOCKER_CMD $MOUNT $DOCKER_IMAGE /bin/sh -c \"$COMPILER$ARGS;$COPY_TO_FINAL\""
    >&2 echo "compiler CMD:$CMD"
    eval $CMD
    echo "local p4 build is in: $BUILD_DIR"
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
        eval "$CMD"
        ;;
    # rebuild and push to docker hub -- will only work for admins
    rebuild_and_push)
        shift 
        check_lucid_dir
        CMD="docker build -t $DOCKER_IMAGE .  && docker push $DOCKER_IMAGE"
        eval "$CMD"
        ;;        
    # get latest from docker hub
    pull)
        shift
        CMD="docker pull $DOCKER_IMAGE"
        eval "$CMD"
        ;;
    interp)
        shift
        interpret_cmd "$@"
        ;;
    interpret)
        shift
        interpret_cmd "$@"
        ;;
    compile)
        shift
        compile_cmd "$@"
        ;;
    *)
        echo "usage: ./lucid.sh <interpret | compile> <arguments to lucid interpreter or compiler>"
        exit 1;
        ;;
esac

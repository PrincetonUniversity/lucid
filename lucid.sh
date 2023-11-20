#### Dockerize lucid build environment ####
# This script compiles and executes Lucid in a docker image, 
# then prints or copies the output back to your machine. 

# requirements: docker

USAGE=$(cat <<-END
Usage:
./lucid.sh interp <prog.dpt> -- run lucid interpreter inside of the docker image.
./lucid.sh compile <prog.dpt> -- run lucid compiler inside the docker image.
./lucid.sh rebuild -- rebuild the lucid compiler in your docker image, from this local repo.
./lucid.sh pull -- pull most recently published version of lucid image.
./lucid.sh enter_dev -- enter a lucid development image where the local repo is mounted at /lucid.
./lucid.sh pull_dev -- pull most recently published version of lucid dev image.
./lucid.sh rebuild_and_push -- rebuild docker image and publish (admin only)
./lucid.sh rebuild_and_push_dev -- rebuild docker image and publish (admin only)
END

)

DOCKER_IMAGE="jsonch/lucid:lucid"
DOCKER_DEV_IMAGE="jsonch/lucid:lucid_dev"
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
        echo 1 > /dev/null
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
# we can't mount all the files at once, because 
# we don't know which files to mount yet
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
    # command to get includes of curent file
    GICMD="docker run --rm -it --mount type=bind,source=$local,target=$remote $DOCKER_IMAGE /bin/sh -c \"$DOCKER_UTIL $next\""
    # >&2 echo "GICMD:$GICMD"
    next_includes=$(eval "$GICMD")
    rv=$?
    if [ "$rv" -ne "0" ]; then
        >&2 echo "*** docker wrapper error *** parse error in file: $next"
        >&2 echo "$next_includes"
        >&2 echo "*** end docker wrapper error ***"
        exit $rv
    fi
    next_includes=$(strip_whitespace $next_includes)
    # `docker run --rm -it --mount type=bind,source=$local,target=$remote $DOCKER_IMAGE /bin/sh -c "bin/dockerUtils includes $next"`
    # >&2 echo "result: $next_includes"
    echo "$next_includes"
    return 0
}

# get included files, recursively to any depth
get_all_includes()
{
    src=$1
    # >&2 echo "get_all_includes is processing: $src"
    src_includes_str=$(get_includes $src)
    rv=$?; if [ "$rv" -ne "0" ]; then return $rv; fi
    # make sure the operation was a success.
    IFS=':' read -ra incl_arr <<< "$src_includes_str"
    for dep_src in "${incl_arr[@]}"; do
        # >&2 echo "get_all_includes is processing dependency: $dep_src"
        # get dependencies of dependent
        recursive_includes_str=$(get_all_includes $dep_src)
        rv=$?
        if [ "$rv" -ne "0" ]; then return $rv; fi
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
    return 0
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
    rv=$?; if [ "$rv" -ne "0" ]; then return $rv; fi

    all_files_str="$main:$all_includes_str"
    IFS=':' read -ra all_files_arr <<< "$all_files_str"

    # unique elements of array 
    IFS=" " read -r -a all_files_arr <<< "$(echo "${all_files_arr[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' ')"
    echo "${all_files_arr[@]}"
    return 0
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
    rv=$?; if [ "$rv" -ne "0" ]; then return $rv; fi
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
    rv=$?; if [ "$rv" -ne "0" ]; then return $rv; fi
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
                # --spec)
                #     SPEC_FOUND=1
                #     ARGS="$ARGS --spec $(docker_local_fn ${2})"
                #     MOUNT="$MOUNT $(mount_file_str ${2})"
                #     shift
                #     shift
                #     ;;            
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
    # if [[ $SPEC_FOUND == 0 ]]
    # then
    #     >&2 echo "no interp spec file provided"
    #     spec=${main%.dpt}.json
    #     >&2 echo "checking if spec exists: $spec"
    #     if [[ -f "$spec" ]]
    #     then
    #         >&2 echo "spec exists, adding args"
    #         ARGS="$ARGS --spec $(docker_local_fn ${spec})"
    #         MOUNT="$MOUNT $(mount_file_str ${spec})"
    #     fi
    # fi
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
    # rebuild lucid -- run when you change lucid's source code.
    rebuild)
        shift 
        check_lucid_dir
        # note: this command builds the final image defined in the local dockerfile (.) and tags it as
        # $DOCKER_IMAGE. _even if the dockerfile does not name the final image $DOCKER_IMAGE_.
        CMD="docker build -t $DOCKER_IMAGE ."
        eval "$CMD"
        ;;
    # (admin only) rebuild a new lucid binary and push to docker hub.
    # note: this relies on the dev image, so if the dependencies have changed, 
    # that image should be updated first.
    rebuild_and_push)
        shift 
        check_lucid_dir
        CMD="docker build -t $DOCKER_IMAGE .  && docker push $DOCKER_IMAGE"
        eval "$CMD"
        ;;        
    # enter an instance of the lucid dev image 
    # with the directory that this script is in mounted to /lucid
    enter_dev)
        # directory that this script is in
        SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
        # directory of the lucid repo
        mount_args="-v $SCRIPT_DIR:/lucid"
        cmd="$DOCKER_CMD --workdir /lucid $mount_args $DOCKER_DEV_IMAGE"
        # echo "command: $cmd"
        eval "$cmd"
        ;;
    # pull the base version of the dev image. Use this if you mess up 
    # your local copy of the dev image
    pull_dev)
        shift
        CMD="docker pull $DOCKER_DEV_IMAGE"
        eval "$CMD"
        ;;
    # (admin only) rebuild the lucid dev image and push to public repo -- 
    # the dev image is used to build lucid for the main image, and for 
    # local development. It does not have a local copy of lucid, 
    # but rather just lucid's dependencies. 
    # So the dev image should only change when lucid's dependencies change.
    rebuild_and_push_dev)
        shift 
        check_lucid_dir
        eval "docker build --target lucid_dev --tag jsonch/lucid:lucid_dev . && docker push jsonch/lucid:lucid_dev"
        ;;
    *)
        echo "unknown command argument."
        echo "$USAGE"
        exit 1;
        ;;


esac

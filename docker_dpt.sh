# This runs ./dpt inside of an ubuntu 18.04 docker image. 
# WARNING: THIS SCRIPT WILL ONLY WORK IF IT IS 
# This script only works if it is in the root directory of the lucid git
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# check if this is the root directory by looking for the opam file.
if [ -f "$SCRIPT_DIR/dpt.opam" ]; 
then
    continue;
else
    exit 1;
fi

# build the ocamlbase docker image if it doesnt exist
if [[ "$(docker images -q ocamlbase 2> /dev/null)" == "" ]]; then
  echo "building ocamlbase docker image"
  docker build -t ocamlbase "$SCRIPT_DIR/docker/ocamlbase"
fi


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
        ABSOLUTE_PATH=$(cd $(dirname {var}); pwd)
        echo $ABSOLUTE_PATH
    elif [ -f "${var}" ]
    then 
        ABSOLUTE_PATH=$(cd $(dirname {var}); pwd)
        BASE=$(basename ${var})
        echo $ABSOLUTE_PATH/$BASE
    else 
        echo $var
    fi
}



# command to run docker image, compile lucid, then execute it
CMD="docker run --rm -it -v $SCRIPT_DIR:/home/opam/lucid"

# arguments to dpt inside of docker
ARGS=""
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

CMD="$CMD jsonch/lucid:ocamlbase /bin/bash -c \"cd /home/opam/lucid; make; ./dpt $ARGS\""

# docker run --rm -it -v /Users/jsonch/Desktop/gits/lucid:/home/opam/lucid -v /Users/jsonch/Desktop/gits/lucid/mac_learner.dpt:/home/opam/inputs/mac_learner.dpt -v /Users/jsonch/Desktop/gits/lucid/mac_learner.json:/home/opam/inputs/mac_learner.json ocamlbase /bin/bash

echo "running command: $CMD"
eval "$CMD"
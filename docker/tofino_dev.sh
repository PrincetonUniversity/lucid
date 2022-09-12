# enter the full lucid-tofino dev docker, 
# with the lucid repo mounted at /lucid

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# build the docker image if it doesnt exist
if [[ "$(docker images -q tofinobase 2> /dev/null)" == "" ]]; then
  echo "building tofinobase docker image"
  docker build -t tofinobase ./tofinobase
fi

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

to_gparent_dir()
{
    cd $1 && cd ../ && pwd
}

LUCID_GIT_DIR=$(to_gparent_dir "$SCRIPT_DIR")
docker start tofinobase || docker run --privileged --rm -it -v $LUCID_GIT_DIR:/lucid tofinobase
# build and run a docker container for lucid-tofino development.
# To use this script, you must have a copy of the bf-sde tarball from Intel, or a copy of the docker image tarball from the lucid devs.

# Instructions: 

# A) to load the docker image from a tarball
# 1) install docker (use docker desktop if you have a gui)
# 2) run "./tofino_dev.sh load tofino_dev.tar" to load the docker image from the tarball

# B) to build the docker image from source
# 1) install docker (use docker desktop if you have a gui)
# 2) put bf-sde-9.7.2.tgz in ./docker/tofino_dev -- note: you must acquire this from Intel yourself
# 3) run "./tofino_dev.sh build" to build the docker image -- requires ~50GB free for the build, 30GB of that will be freed after build finishes.

# C) to use the docker image
# 1) run ./tofino_dev.sh to start the docker container and open a bash session within it
# 2) inside the container, run "cd /lucid" to get to a shared mount of your lucid repo. 
# 3) in other terminal windows, run "./tofino_dev.sh" to open new bash terminals in the same docker container
# 4) use exit to exit the docker terminals.

# commands: 
# no command --     open a bash terminal in the docker container, creating and starting it if necessary (but not building!)
# build --          build a fresh docker image from the Dockerfile at ./tofino_dev/Dockerfile. 
#                   assumes that there is a copy of the appropriate bf-sde tarball at ./docker/tofino_dev/*.tgz
# load <img.tar> -- load a docker image from a tarball
# save <img.tar> -- save the built docker image to a tarball
# create --         create a docker container from the built image
# start  --         launch a created container in the background
# bash   --         run a bash terminal in a launched container
# stop   --         opposite of stop
# delete --         opposite of build

# directory that this script is in
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

image_name="tofino_dev"
container_name="tofino_dev_container"

# docker_run="docker run --privileged -it"
docker_create="docker create --privileged -it "
docker_start="docker start "
docker_stop="docker stop "
docker_exec='docker exec -it '

mount_args="-v $SCRIPT_DIR:/lucid"
dockerfile="docker/$image_name/Dockerfile"
docker_build="docker build"

# build the image
if [[ $1 == "build" ]]
then
    cmd="$docker_build -f $dockerfile -t $image_name ."
    echo "build command: $cmd"
    eval $cmd
# create the container from the image
elif [[ $1 == "create" ]]
then
    echo "creating docker container: $container_name"
    cmd="$docker_create --name $container_name $mount_args $image_name"
    eval $cmd    
elif [[ $1 == "start" ]]
then
    echo "starting docker container: $cmd"
    cmd="$docker_start $container_name"
    eval $cmd
elif [[ $2 == "bash" ]]
then
    echo "opening bash in container: $cmd"
    cmd="$docker_exec $container_name bash"
    eval $cmd
# stop the container, ending all shells
elif [[ $1 == "stop" ]]
then
    cmd="$docker_stop $container_name"
    echo "stopping docker container: $container_name"
    eval $cmd
# delete the container -- 
# WARNING: you will have to start over from a fresh image
# after a delete!
elif [[ $1 == "delete" ]]
then
    echo "deleting / rm-ing $container_name"
    cmd="docker rm $container_name"
    eval $cmd
# save the built image to a tarball
elif [[ $1 == "save" ]]
then
    # make sure there are two arguments
    if [ $# -ne 2 ]
    then
        echo "usage: ./tofino_dev.sh save tofino_dev.tar"
        exit 1
    fi
    # make sure the image exists
    if [ ! "$(docker images -q $image_name)" ]
    then
        echo "cannot save image -- $image_name does not exist"
        exit 1
    fi
    echo "saving to tarball: $2"
    cmd="docker save $image_name -o $2 "
    eval $cmd
# load the image from a tarball
elif [[ $1 == "load" ]]
then 
    # make sure there are two arguments
    if [ $# -ne 2 ]
    then
        echo "usage: ./tofino_dev.sh load tofino_dev.tar"
        exit 1
    fi
    echo "loading from tarball: $2"
    cmd="docker load -i $2"
    eval $cmd
# default: open bash in container, creating and starting it if necessary
else
    createcmd="$docker_create --name $container_name $mount_args $image_name 2&> /dev/null"
    startcmd="$docker_start $container_name > /dev/null"
    bashcmd="$docker_exec $container_name bash"
    if [ ! "$(docker ps -a | grep $container_name)" ]
    then
        echo "creating container: $container_name"
        eval "$createcmd"
    fi
    if [ ! "$( docker container inspect -f '{{.State.Running}}' $container_name )" == "true" ] 
    then
        echo "starting container: $container_name"
        eval "$startcmd"
    fi
    eval "$bashcmd"
fi
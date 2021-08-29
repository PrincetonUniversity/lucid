POSITIONAL=()

function ensure_exist() {
    if [ -e $1 ]
    then
        continue
    else
        echo "error: file $1 does not exist"
        exit 1;
    fi
}
function abs_path() {
    echo "$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"    
}

function my_path() {
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
    echo $SCRIPT_DIR
}

CPUS="1"
DEFAULT_SDE="$(my_path)/bf-sde-9.5.0.tgz"
DEFAULT_SET_SCRIPT="$(my_path)/set_sde.bash"
BOX="lucid.box"

for i in "$@"; do
  case $i in
    -c=*|--cpus=*)
      CPUS="${i#*=}"
      shift # past argument=value
      ;;
    -s=*|--sde=*)
      SDE="${i#*=}"
      echo $SDE
      shift # past argument=value
      ;;
    --set_script=*)
      SET_SCRIPT="${i#*=}"
      shift
      ;;
    -s|--sde)
      SDE=$DEFAULT_SDE
      shift # past argument
      ;;
    *)
      echo "error: unknown option."
      exit 1;
      # unknown option
      ;;
  esac
done


cmd="vagrant"
# check argument validity and build command
case $CPUS in
    ''|*[!0-9]*) echo "error: number of cpus must be...a number." ;;
    *) cmd="$cmd --cpus=$CPUS" ;;
esac
if [ $SDE ]
then 
    # make sure sde exists
    ensure_exist $SDE
    # make sure set script is set
    if [ -z $SET_SCRIPT ]
    then 
        SET_SCRIPT=$DEFAULT_SET_SCRIPT
    fi
    echo `pwd`
    ensure_exist $SET_SCRIPT
    cmd="$cmd --sde=$(abs_path $SDE) --set_script=$(abs_path $SET_SCRIPT)"
fi
cmd="$cmd up --provider=virtualbox"


echo "building vagrant vm"
echo "$cmd"
echo "******************"
$cmd
vagrant reload
echo "clearing unused space for smaller image"
vagrant ssh -c "sudo dd if=/dev/zero of=/EMPTY bs=1M; sudo rm -f /EMPTY"
echo "packaging into $BOX"
vagrant package --output "$BOX"
echo "destroying default box"
vagrant destroy -f
echo "done! lucid vm built in: $BOX"
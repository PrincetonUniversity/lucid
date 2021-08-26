#!/bin/bash
# simple shell to build and run the components of a p4 program + c manager.

# simulation configuration
PORT_DPIDS="0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 128 132 136 140 144 148 152 156 160 164 168 172 176 180 184 188 192 196"
RECIRC_DPID="196"
LOG_DIR="run_logs"
VETH_JSON="sim_veth_map.json"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

function check_env() {
    if [ -z "$SDE_INSTALL" ] ; then
        echo "error: SDE_INSTALL env var must be set"
        exit
    fi
    if [ -z "$SDE" ] ; then
        echo "error: SDE env var must be set"
        exit
    fi
}

# file and directory names 
function strip_path() {
    local BASE_PROG=${1##*/}; 
    echo "$BASE_PROG"
}
function strip_ext() {
    local BASE_PROG=${1%%.*}; 
    echo "$BASE_PROG"    
}

function to_build_dir() {
    local BASE_PROG=$(realpath $1)
    local BASE_PROG=${BASE_PROG%%.*}; 
    echo "$BASE_PROG"
}
# source fn
function to_switchd_fn() {
    local BASE_PROG=$(realpath $1)
    local BASE_PROG=${BASE_PROG%%.*}; 
    echo "${BASE_PROG}.cpp"
}
function to_conf_fn() {
    local BUILD_DIR="$(to_build_dir $1)"
    local CONF_BASE="$(strip_path $BUILD_DIR)";
    local CONF_FN="$BUILD_DIR/$CONF_BASE.conf"
    echo "${CONF_FN}"
}
function to_vethconf_fn() {
    echo "$(realpath $LOG_DIR)/$VETH_JSON"
}
function to_switchd_bin() {
    local BUILD_DIR="$(to_build_dir $1)"
    local BIN_FN="$BUILD_DIR/bf_switchd"
    echo "${BIN_FN}"
}
function to_python_mgr() {
    local BASE_PROG=$(realpath $1)
    local BASE_PROG=${BASE_PROG%%.*}; 
    echo "$BASE_PROG.py"
}
function to_logs_dir() {
    local BASE_PROG=$(realpath $1)
    local SIM_DIR=$(dirname $BASE_PROG)/$LOG_DIR
    echo $SIM_DIR
}

# ================================
# =           building           =
# ================================

# compile a P4 program to a local build directory.
function build_p4() {
    local P4_IN=$1; local BUILD_OUT=$2
    echo "**** compiling $(strip_path $P4_IN) to $(strip_path $BUILD_OUT) ****"
    local P4C="$SDE_INSTALL/bin/bf-p4c"
    if [[ $* == *-v* ]] ; then 
        P4C="$P4C --verbose 3"
    fi

    local P4C_ARGS="" # useful p4c args: --listFrontendPasses --help
    if [ -z "$P4C_ARGS" ] ; then 
        P4C_ARGS=""
    else
        P4C_ARGS="-Xp4c \"${P4C_ARGS}\""
    fi

    local CMD="$P4C --std p4-16 --target tofino --arch tna -o $BUILD_OUT --bf-rt-schema $BUILD_OUT/bf-rt.json $P4_IN $P4C_ARGS"
    echo "cmd: $CMD"
    $CMD
    echo "**** done compiling $(strip_path $P4_IN) to $(strip_path $BUILD_OUT) ****"
}

# compile a custom bf_switchd agent to a local build directory.
function build_mgr() {
    local CTL_SRC=$1; local BUILD_OUT=$2
    local BIN="$BUILD_OUT/bf_switchd"
    echo "**** compiling $(strip_path $CTL_SRC) to $(strip_path $BUILD_DIR)/$(strip_path $BIN) ****"
    BF_DRV_SRC=${SDE}/pkgsrc/bf-drivers
    local CMD="g++ -I${BF_DRV_SRC}/include -I${SDE_INSTALL}/include -Wno-missing-field-initializers -Werror -Wshadow -g -O2 -std=c++11 -o ${BIN} ${CTL_SRC} -ldriver -lbfsys -lbf_switchd_lib -lpifeproto -lpiall -lm -ldl -lpthread -pthread -Wl,--disable-new-dtags -L${SDE_INSTALL}/lib -Wl,-rpath -Wl,${SDE_INSTALL}/lib"
    echo "CMD: $CMD"
    $CMD
    echo "**** done compiling $(strip_path $CTL_SRC) to $(strip_path $BUILD_DIR)/$(strip_path $BIN) ****"
}

# compile a P4 program and its custom bf_switchd agent to the local build directory.
function build() {
    P4_SRC=$(realpath "$1")
    CTL_SRC=$(to_switchd_fn "$1")
    BUILD_DIR=$(to_build_dir "$1")
    rm -rf "$BUILD_DIR"; mkdir -p "$BUILD_DIR"
    build_p4 "$P4_SRC" "$BUILD_DIR" "-v" $3 && build_mgr "$CTL_SRC" "$BUILD_DIR"
}


# ======  End of building  =======


# ===============================
# =           running           =
# ===============================

# setup veths for tofino simulator and create a matching json config file.
function config_veth() {
    sudo ip link set dev $1 up > /dev/null 2>&1
    sudo ifconfig $1 mtu 10240 up > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 rx off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 tx off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 sg off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 tso off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 ufo off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 gso off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 gro off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 lro off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 rxvlan off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 txvlan off > /dev/null 2>&1
    sudo /sbin/ethtool --offload $1 rxhash off > /dev/null 2>&1
    sudo sysctl net.ipv6.conf.$1.disable_ipv6=1 > /dev/null 2>&1
}

create_veth_pair() {
    echo "creating veth pair $vethA <--> $vethB"
    sudo ip link add name $1 type veth peer name $2
    config_veth $1
    config_veth $2
}

function dpid_to_host_veth() {
    echo "veth$(( $1 * 2 + 1 ))"
}

create_veth_pairs() {
    echo "setting up veths for ports in simulator"
    json_str="{\"PortToVeth\": ["
    for i in $PORT_DPIDS; do 
        A=$(( i * 2 ))
        B=$(( i * 2 + 1 ))
        vethA=veth$(( i * 2 ))
        vethB=veth$(( i * 2 + 1 ))
        json_str="$json_str{\"device_port\":$i, \"veth1\":$A, \"veth2\":$B},"
        (ip link show $vethA > /dev/null 2>&1 && echo "veth pair $vethA <--> $vethB exists") || create_veth_pair $vethA $vethB
    done
    json_str="$json_str]}"
    echo $json_str > $(to_vethconf_fn)
}

# multi-threading helpers
function run_prog() {
    PROG="unbuffer $1"
    SIG_STR=$2
    PREFIX=$3
    $PROG |& 
        while read line; 
            do 
                if [[ "$line" == *"$SIG_STR"* ]]; then
                    kill -USR1 $$
                fi
                echo "[$PREFIX] $line"; 
            done
}

# launch $1 in the background, wait until it prints $2 to continue. 
# also, print its output prefixed with [$3]
function launch_and_wait() {    
    trap 'started=1' USR1
    started=0
    (run_prog "$1" "$2" "$3") &
    while [ "$started" -ne 1 ]; do
        sleep 1
    done
}
function cd_launch_and_wait() {    
    trap 'started=1' USR1
    started=0
    (cd "$1"; run_prog "$2" "$3" "$4") &
    while [ "$started" -ne 1 ]; do
        sleep 1
    done
}

function start_asic_sim() {
    local P4_CONF=$1
    local SIMULATOR="sudo $SDE_INSTALL/bin/tofino-model"

    # setup veths for simulator
    create_veth_pairs 

    # launch simulator in background, wait for ready notification to continue.
    local SIM_CMD="$SIMULATOR --p4-target-config $P4_CONF -d 1 -f $(to_vethconf_fn) --chip-type 2 --install-dir $SDE_INSTALL --log-dir . --json-logs-enable --pkt-log-len 100000 --int-port-loop $RECIRC_DPID"
    local SIG_STR="Waiting for incoming connections..."
    echo "SIM_CMD: $SIM_CMD"
    cd_launch_and_wait "$LOG_DIR" "$SIM_CMD" "$SIG_STR" "SIM"
}
# sudo ./${PROG} --install-dir $SDE_INSTALL --conf-file $SDE_INSTALL/share/p4/targets/tofino/$PROGNAME.conf

function start_switchd() {
    local CMD="sudo $2 --install-dir $SDE_INSTALL --conf-file $1"
    local SIG="bfruntime gRPC server started on"
    echo $CMD
    cd_launch_and_wait "$LOG_DIR" "$CMD" "$SIG" "SWITCHD_MGR"
}

function start_python() {
    local CMD="python2 $1"
    local SIG="mgr.py disconnect complete."
    launch_and_wait "$CMD" "$SIG" "PY_MGR"
}

# start up the asic simulator and switchd for a built project.
function startsim() {
    local CONF_FN=$(to_conf_fn "$1")
    local MGR_BIN=$(to_switchd_bin "$1")
    local MGR_PY=$(to_python_mgr "$1")

    echo "running simulator on: $CONF_FN"    
    rm -rf "$LOG_DIR"; mkdir -p "$LOG_DIR"

    # start the asic simulator
    start_asic_sim "$CONF_FN"
    SIM_PID=$!

    # start the switchd program
    start_switchd "$CONF_FN" "$MGR_BIN"
    SWITCHD_PID=$!

    # run the python manager script
    start_python "$MGR_PY"    
}
function stopsim() {
    echo "**** stopping simulator and switchd! ****"
    pkill -P $SWITCHD_PID
    pkill -P $SIM_PID    
}

function runsim() {
    startsim $1
    # wait for ctl + c or exit
    echo "**** simulation running -- press ctrl+c to terminate. ****"
    trap 'stopsim' 9
    wait $SWITCHD_PID
}

function starthw() {
    local CONF_FN=$(to_conf_fn "$1")
    local MGR_BIN=$(to_switchd_bin "$1")
    local MGR_PY=$(to_python_mgr "$1")
    echo "running on: $CONF_FN"    

    # start the switchd program
    start_switchd "$CONF_FN" "$MGR_BIN"
    SWITCHD_PID=$!

    # run the python manager script
    start_python "$MGR_PY"    
}
function stophw() {
    echo "**** stopping switchd! ****"
    pkill -P $SWITCHD_PID
}
function runhw() {
    starthw $1
    # wait for ctl + c or exit
    echo "**** switchd running -- press ctrl+c to terminate. ****"
    trap 'stophw' 9
    wait $SWITCHD_PID
}
# ======  End of running  =======

# ===============================
# =           testing           =
# ===============================

function pcapfn_of_jsonfn() {
    echo "$(strip_ext $1).pcap"
}
function rx_pcapfn_of_jsonfn() {
    echo "$(strip_ext $1).rx.pcap"
}
function tracefn_of_p4fn() {
    echo "$(strip_ext $1).json"
}

# craft the pcap 
function generate_pcap() {
    local jsonfn=$1; local pcapfn=$2
    python3 $SCRIPT_DIR/generate.py "$jsonfn" "$pcapfn"
}

# capture pcap on an interface. Print pid to kill later.
function capture_pcap() {
    local rx_pcap_fn=$1; local if_in=$2
    local cmd="sudo tcpdump -U -Q in -i $if_in -w $rx_pcap_fn"
    $cmd > /dev/null 2> /dev/null &
    echo $!
}
# send pcap on an interface
function send_pcap() {
    local pcap_fn=$1; local if_out=$2
    echo "sending out of $if_out"
    local cmd="sudo tcpreplay --preload-pcap --quiet -i $if_out $pcapfn"
    echo "send_pcap cmd: $cmd"
    $cmd
}

# usage (after everything is running) : runtest <p4 prog name> <output dpid> [<output dpid>]
function runtest() {
    local jsonfn="$(tracefn_of_p4fn $1)" # infer based on P4 program name. Will also work with *.json
    local pcapfn="$(to_logs_dir $1)/$(pcapfn_of_jsonfn $1)"
    local if_out="$(dpid_to_host_veth $2)"
    if [ -z "$3" ] ; then
        generate_pcap "$jsonfn" "$pcapfn"
        send_pcap "$pcap_fn" "$if_out"
    else
        local if_in="$(dpid_to_host_veth $3)"
        local rx_pcapfn="$(to_logs_dir $1)/$(rx_pcapfn_of_jsonfn $1)"
        tcpdump_pid=$(capture_pcap "$rx_pcapfn" "$if_in")
        sleep 1
        generate_pcap "$jsonfn" "$pcapfn"
        send_pcap "$pcap_fn" "$if_out"
        sleep 2
        echo "killing tcpdump (pid $tcpdump_pid)"
        sudo kill -9 $tcpdump_pid
    fi
}

# ======  End of testing  =======


function main() {
    check_env
    case $1 in 
        "build") shift; build $@
        ;;
        "sim") shift; runsim $@
        ;;
        "hw") shift; runhw $@
        ;;
        "test") shift; runtest $@
        ;;
        *) echo "usage: p4_build.sh <build | sim | hw | test> <p4 fn>"
        ;;
    esac
}

main $@

#!/bin/bash
# bash script to build and run the components of a p4 program on a tofino model and switch.
# must be run from the build directory produced by the lucid-tofino compiler.

# the current user
USER=$(whoami)
PROJ_DIR="$(realpath .)"

# simulation configuration
PORT_DPIDS="128 129 130 131 132 136 140 144 148 152 156 160 164 168 172 176 180 184 188 192"
RECIRC_DPID="196"
LOG_DIR="run_logs"
MODEL_LOG_BASE="model_"
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
    echo "${BASE_PROG}_tofino"
}
# source fn
function to_switchd_fn() {
    local BASE_PROG=$(realpath $1)
    local BASE_PROG=${BASE_PROG%%.*}; 
    echo "${BASE_PROG}.cpp"
}
function to_conf_fn() {
    local BASE_PROG=$(strip_path $1)
    local BASE_PROG=$(strip_ext $BASE_PROG)
    local BUILD_DIR="$(to_build_dir $1)"
    local CONF_FN="$BUILD_DIR/$BASE_PROG.conf"
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
    local LDIR=$(dirname $BASE_PROG)/$LOG_DIR
    echo $LDIR
}
function to_model_log() {
    local LDIR=$(to_logs_dir $1)
    local LFN=($LDIR/$MODEL_LOG_BASE*)
    echo $LFN

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
    local P4C_ARGS="--table-placement-in-order"
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
    # local CMD="g++ -I${BF_DRV_SRC}/include -I${SDE_INSTALL}/include -Wno-missing-field-initializers -Werror -Wshadow -g -O2 -std=c++11 -o ${BIN} ${CTL_SRC} -ldriver -lbfsys -lbf_switchd_lib -lpifeproto -lpiall -lm -ldl -lpthread -pthread -Wl,--disable-new-dtags -L${SDE_INSTALL}/lib -Wl,-rpath -Wl,${SDE_INSTALL}/lib"
    # 9.7.0 -- rename libbf_switchd --> libbf_switch
    # local CMD="g++ -I${BF_DRV_SRC}/include -I${SDE_INSTALL}/include -Wno-missing-field-initializers -Werror -Wshadow -g -O2 -std=c++11 -o ${BIN} ${CTL_SRC} -ldriver -lbfsys -lbf_switch -lpifeproto -lpiall -lm -ldl -lpthread -pthread -Wl,--disable-new-dtags -L${SDE_INSTALL}/lib -Wl,-rpath -Wl,${SDE_INSTALL}/lib"
    # remove un-needed libraries
    local CMD="g++ -I${BF_DRV_SRC}/include -I${SDE_INSTALL}/include -Wno-missing-field-initializers -Werror -Wshadow -g -O2 -std=c++11 -o ${BIN} ${CTL_SRC} -ldriver -lbfsys -lbf_switch -lm -ldl -lpthread -pthread -Wl,--disable-new-dtags -L${SDE_INSTALL}/lib -Wl,-rpath -Wl,${SDE_INSTALL}/lib"
    echo "CMD: $CMD"
    $CMD
    echo "**** done compiling $(strip_path $CTL_SRC) to $(strip_path $BUILD_DIR)/$(strip_path $BIN) ****"
}

# compile a P4 program and its custom bf_switchd agent to the local build directory.
function build() {
    P4_SRC=$(realpath "$1")
    # CTL_SRC=$(to_switchd_fn "$1")
    BUILD_DIR=$(to_build_dir "$1")
    sudo rm -rf "$BUILD_DIR"; mkdir -p "$BUILD_DIR"; mkdir -p "$BUILD_DIR/pcap_output"
    build_p4 "$P4_SRC" "$BUILD_DIR" "-v" $3 # && build_mgr "$CTL_SRC" "$BUILD_DIR"
}

function build_quiet() {
    P4_SRC=$(realpath "$1")
    # CTL_SRC=$(to_switchd_fn "$1")
    BUILD_DIR=$(to_build_dir "$1")
    rm -rf "$BUILD_DIR"; mkdir -p "$BUILD_DIR"
    build_p4 "$P4_SRC" "$BUILD_DIR" $3 # && build_mgr "$CTL_SRC" "$BUILD_DIR"
}

# ======  End of building  =======


# ===============================
# =           simulation setup           =
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

# get a userspace veth from a tofino dpid
function dpid_to_host_veth() {
    echo "veth$(( $1 * 2 + 1 ))"
}

# generate a configuration file for the tofino model 
# that maps dpids to veth pairs
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
    # one last iteration to set up the recirculation port.
    i=$RECIRC_DPID
    A=$(( i * 2 ))
    B=$(( i * 2 + 1 ))
    vethA=veth$(( i * 2 ))
    vethB=veth$(( i * 2 + 1 ))
    json_str="$json_str{\"device_port\":$i, \"veth1\":$A, \"veth2\":$B}"
    (ip link show $vethA > /dev/null 2>&1 && echo "veth pair $vethA <--> $vethB exists") || create_veth_pair $vethA $vethB

    json_str="$json_str]}"
    echo $json_str > $(to_vethconf_fn)
}

function print_userspace_veths() {
    echo "tofino dpid,userspace veth" 
    for i in $PORT_DPIDS; do 
        echo "$i,$(dpid_to_host_veth $i)"
    done

}

# ========================================
# =           running programs           =
# ========================================


# multi-threading helpers
# run program, print stdout to stdout prefixed with
# PREFIX, and when SIG_STR appears, move program 
# to background and return 
function run_prog() {
    trap 'exit' 2
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
    local SIM_CMD="$SIMULATOR --time-disable --p4-target-config $P4_CONF -d 1 -f $(to_vethconf_fn) --chip-type 2 --install-dir $SDE_INSTALL --log-dir . --json-logs-enable --pkt-log-len 100000 --int-port-loop $RECIRC_DPID"
    local SIG_STR="Waiting for incoming connections..."
    echo "SIM_CMD: $SIM_CMD"
    cd_launch_and_wait "$LOG_DIR" "$SIM_CMD" "$SIG_STR" "SIM"
}
# sudo ./${PROG} --install-dir $SDE_INSTALL --conf-file $SDE_INSTALL/share/p4/targets/tofino/$PROGNAME.conf

# start the bf_switchd included with sde
function start_switchd() {
    local CMD="sudo env SDE=$SDE SDE_INSTALL=$SDE_INSTALL PATH=$SDE_INSTALL/bin:$PATH LD_LIBRARY_PATH=/usr/local/lib:$SDE_INSTALL/lib:$LD_LIBRARY_PATH $SDE_INSTALL/bin/bf_switchd --background --status-port 7777 --install-dir $SDE_INSTALL --conf-file $1"
    # local CMD="sudo $SDE_INSTALL/bin/bf_switchd --install-dir $SDE_INSTALL --conf-file $1"
    local SIG="bf_switchd: server started - listening on port 9999"
    echo "SWITCHD COMMAND: $CMD"
    cd_launch_and_wait "$LOG_DIR" "$CMD" "$SIG" "SWITCHD_MGR"
}

# run the control script through bf_cli
function start_python() {
    local CMD="$SDE/run_bfshell.sh -b $1"
    local SIG="control startup complete."
    echo "PYTHON CONTROL: $CMD"
    launch_and_wait "$CMD" "$SIG" "PY_MGR"
}

# =========================================
# =           toplevel commands           =
# =========================================

function startsim() {
    local CONF_FN=$(to_conf_fn "$1")
    # local MGR_BIN=$(to_switchd_bin "$1")
    local MGR_PY=$(to_python_mgr "$1")

    echo "running simulator on: $CONF_FN"    
    rm -rf "$LOG_DIR"; mkdir -p "$LOG_DIR"

    # start the asic simulator
    start_asic_sim "$CONF_FN"
    SIM_PID=$!

    # start the switchd program
    start_switchd "$CONF_FN"
    SWITCHD_PID=$!

    # run the python manager script
    start_python "$MGR_PY"    
}

# stop the simulator and switchd. The control program should exit 
# on its own. After stopping the simulator, set the log directory's user 
# to the current user.
function stopsim() {
    echo "**** stopping simulator and switchd ****"
    { sudo pkill --signal 2 -P $SWITCHD_PID && wait $SWITCHD_PID; } 2>/dev/null
    { sudo pkill --signal 2 -P $SIM_PID && wait $SIM_PID; } 2>/dev/null
    # change the log directory's owner to the current user
    sudo chown -R $(whoami) "$PROJ_DIR"
}
# use this to clean up an aborted run
function killsim() {
    sudo killall bf_switchd
    sudo killall tofino-model
}
# start sim and wait
function runsim() {
    startsim $1
    # wait for ctl + c or exit
    echo "**** ports available in simulation ****"
    print_userspace_veths
    echo "**** simulation running -- press ctrl+c to terminate. ****"
    trap 'stopsim' 9
    wait $SWITCHD_PID
}

function starthw() {
    local CONF_FN=$(to_conf_fn "$1")
    local MGR_BIN=$(to_switchd_bin "$1")
    local MGR_PY=$(to_python_mgr "$1")
    echo "running on: $CONF_FN"    

    rm -rf "$LOG_DIR"; mkdir -p "$LOG_DIR"

    # start switchd
    start_switchd "$CONF_FN"
    SWITCHD_PID=$!

    # run the python manager script
    echo "starting controller: "
    echo "start_python $MGR_PY"
    start_python "$MGR_PY"
}
function stophw() {
    echo "**** stopping switchd! ****"
    pkill --signal 2 -P $SWITCHD_PID
}
function runhw() {
    starthw $@
    # wait for ctl + c or exit
    echo "**** switchd running -- press ctrl+c to terminate. ****"
    trap 'stophw' 9
    wait $SWITCHD_PID
}

# ======  End of running  =======

# ===============================
# =           testing           =
# ===============================

function pcapfn_of() {
    echo "$(strip_ext $1).pcap"
}
function rx_pcapfn_of() {
    echo "$(strip_ext $1).rx.pcap"
}
function tracefn_of() {
    echo "$(strip_ext $1).trace.json"
}
# capture pcap on an interface. Print pid to kill later.
function capture_pcap() {
    local rx_pcap_fn=$1; local if_in=$2
    sudo rm -f "$rx_pcap_fn"
    local cmd="sudo tcpdump -U -Q in -i $if_in -w $rx_pcap_fn"
    $cmd > /dev/null 2> /dev/null &
    echo $!
}
# send pcap on an interface
function send_pcap() {
    local pcap_fn=$1; local if_out=$2
    echo "sending $pcap_fn out of $if_out"
    local cmd="sudo tcpreplay --pps .2 --preload-pcap --quiet -i $if_out $pcap_fn"
    echo "send_pcap cmd: $cmd"
    $cmd
}

# send pcap $1 into the veth interface for $2, record what comes out of $3 
function send_and_collect_pcap() {
    local tx_pcapfn="$1"
    local if_in="$(dpid_to_host_veth $2)"
    local if_out="$(dpid_to_host_veth $3)"
    local rx_pcapfn="$(rx_pcapfn_of $tx_pcapfn)"
    echo "starting tcpdump..."
    tcpdump_pid=$(capture_pcap "$rx_pcapfn" "$if_in")
    sleep 1
    echo "sending pcap..."
    send_pcap "$tx_pcapfn" "$if_out"
    sleep 2
    echo "killing tcpdump (pid $tcpdump_pid)..."
    sudo kill -9 $tcpdump_pid
}


# generate a pcap from the test spec ($2), start the 
# asic simulator, send the pcap, and analyze the 
# compiler output log to see if the test spec's 
# conditions were satisfied. 
function test() {
    local p4fn="$1"
    local jsonfn="$2"
    local pcapfn="trace.pcap"
    # figure out port to send packets out of
    local dpid_in=$(python3 $SCRIPT_DIR/testutils.py input_port "$jsonfn")   
    local port_in=$(dpid_to_host_veth $dpid_in)
    local num_pkts_in=$(python3 $SCRIPT_DIR/testutils.py len_packets "$jsonfn")
    # # generate the pcap
    echo "**** generating test pcap with $num_pkts_in pkts from test spec ****"
    python3 $SCRIPT_DIR/testutils.py gen_pcap "$jsonfn" "$pcapfn"
    # start the simulator
    echo "**** starting tofino model and p4 program ****"
    startsim "$p4fn"
    trap 'stopsim' 9
    echo "**** sending pcap ****"
    send_pcap "$pcapfn" "$port_in"
    # give the model at least 10 seconds to process each packet
    # echo "**** sleeping for $(num_pkts_in) ****"
    # sleep "$num_pkts_in"
    sleep 5
    echo "**** cleaning up ****"
    stopsim
    echo "**** checking test spec against model log ****"
    MODEL_LOG_FN="$(to_model_log $1)"
    result=$(python3 $SCRIPT_DIR/model_log_utils.py check "$jsonfn" "$MODEL_LOG_FN")
    if [ $result = "True" ]; then
        echo "PASS"
        exit 0
    else
        echo "FAIL"
        exit 1
    fi
}


# ======  End of testing  =======


function main() {
    check_env
    case $1 in 
        "build") shift; build $@
        ;;
        "build_quiet") shift; build_quiet $@
        ;;
        "sim") shift; runsim $@
        ;;
        "hw") shift; runhw $@
        ;;
        "send_and_collect_pcap") shift; send_and_collect_pcap $@
        ;;
        "test") shift; test $@
        ;;
        "simtest") shift; simtest $@
        ;;
        "killsim") shift; killsim $@
        ;;
        *) echo "usage:"; 
           echo "p4tapp.sh <build | build_quiet | sim | hw> prog.p4";
           echo "p4tapp.sh send_and_collect_pcap trace.pcap dpid_in dpid_out"
           echo "p4tapp.sh simtest prog.p4 trace.pcap dpid_in dpid_out"
           echo "p4tapp.sh test prog.p4 testspec.json"

           echo "p4tall.sh killsim"
        ;;
    esac
}

main $@

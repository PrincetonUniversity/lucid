#
# Basic usage check
#
if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    echo ERROR: This script is supposed to be sourced, not executed
    exit 1
fi
# set_sde.sh
# SDE selection script
#
# (c) Barefoot Networks, 2017
# All Rights Reserved
#
# Usage:
# ------
#
# Source this file in BASH to select the desired version of Barefoot SDE
# For example:
#    . ~/tools/set_sde.bash
# or
#    source ~/tools/set_sde.bash
#
# NOTE: Because this file is supposed to be SOURCED and not EXECUTED
#       do NOT make it executable. This will prevent unnecessary errors and
#       frustrations :)
#
# Quick Manual:
# =============
#    You can source this file from any directory. It will try to search
#    for SDE directory as follows:
#      1) In the current directory. If this directory contains SDE, it
#         will be chosen (so the behavior is like before)
#         **** This is the main method that always works ****
#
#      2) In the directories ~/bf-sde-*.*.* in reverse chronological
#         order (i.e. starting from the newest).
#
#         Note: if you keep your SDE trees somewhere else, e.g. in
#               /opt/src then you can simply replace '~' with /opt/src
#         **** This is a convenience method that might require customization
#    
#    It also prints the chosen SDE version (or an error if nothing is
#    found). This reduces the potential for errors.
#
# CAVEAT: If you try to source this file from an SDE subdirectory (e.g.
#         ~/bf-sde-x.y.z.w/pkgsrc, the script might or might not choose
#         bf-sde-x.y.z.w, based on whether this is a latest SDE or not. 
#
#
# Additional commands
# -------------------
#
# The script also defines additional commands that you might find helpful in
# your day-to-day work with the SDE.
#
# set_sde
#       Sets the environment as described above; This is especially useful
#       if you decide to incorporate this script in your .bashrc
# 
# sde
#        Changes directory to $SDE. No parameters.
#
#        A useful paradigm is:
#            . ~/tools/set_sde.bash; sde
#
#        Easy to type on the terminal and then copy to others in one piece
#
#
# build_dir <program_name or the path to the main program .p4 file> [suffix]
#        Prints the full pathname of the directory that is created for
#        the given program by p4-build.sh script.
#
#        Typical usage:
#            cd `build_dir myprog` or cd `build_dir p4src/myprog.p4`
#            cd `build_dir myprog .p4c`   (with suffix ".p4c")
#
#        Note: this command is aware the typical build_dir for standard
#        programs switch.p4 and diag.p4 are different
#
#        Suffix is the optional parameter and corresponds to the suffix,
#        used by p4_build.sh script
#
#
# tofino_dir <program_name or the path to the main program .p4 file> [suffix]
#        Prints then full pathname of the directory, containing the
#        p4c-tofino compiper output. Essentially it is equivalent to
#        `build_dir prog>`/tofino/prog
#
#
# logs_dir <program_name or the path to the main program .p4 file> [suffix]
#       Prints the full name of the directory, containing the log files
#       produced by p4c-tofino
#
#       Typical usage:
#           vi `logs_dir myprog`/pa.results.log
#
#
# vis_dir <program_name or the path to the main program .p4 file> [suffix]
#       Prints the full name of the directory, containing the visualizations
#       produced by p4c-tofino
#
#       Typical usage:
#           firefox `vis_dir myprog`
#
#
# graphs_dir  <program_name or the path to the main program .p4 file> [suffix]
#       Prints the full name of the directory, containing the visualizations
#       produced by p4c-tofino
#
#       Typical usage:
#           mkdir -p `graphs_dir myprog`
#           ~/tools/p4-graphs-tofino myprog.p4 --gen-dir `graphs_dir myprog`
#
# pd.h <program_name or the path to the main program .p4 file> [suffix]
#       Prints the full name of the pd.h, generated for the given program
#
#       Typical usage:
#           emacs `pd.h myprog` &
#
# context.json <program_name or the path to the main program .p4 file> [suffix]
#       Prints the full name of the context.json, generated for the given
#       program
#
#       Typical usage:
#           kate `context.json myprog` &
############################################################################

function set_sde() {
    local sdes=~   # Directory, where you keep SDE trees, $HOME, /opt/sde, etc.
    
    for sde_dir in `pwd` `/bin/ls -dt ${sdes}/bf-sde-*.*.* 2> /dev/null`; do
        manifest=`/bin/ls $sde_dir/*.manifest 2> /dev/null | head -n 1`
        if [ ! -z $manifest ]; then
            export SDE=$sde_dir
            export SDE_INSTALL=$SDE/install
            export PATH=$SDE_INSTALL/bin:$PATH
            echo Using `basename $manifest .manifest` in $SDE
            break
        fi
    done

    if [ -z $manifest ]; then
        echo "ERROR: No suitable SDE directory found"
        echo "       Please, source this file from the root of your SDE directory"
    fi
}

set_sde

function sde() {
    cd $SDE
}


function build_dir() {
    prog=$1; shift
    suffix=$1; shift
    
    SDE_BUILD=$SDE/build

    # Remove occasional directory and extension to get the name
    prog=`basename $prog .p4`
    if [ ! -z $prog ]; then
        if [ $prog = switch ]; then
            if [ -d $SDE_BUILD/$prog$suffix/p4-build ]; then
                echo $SDE_BUILD/$prog$suffix/p4-build
                return 0
            fi
        fi
        
        if [ $prog = diag ]; then
            if [ -d $SDE_BUILD/$prog$suffix/p4-build ]; then
                echo $SDE_BUILD/$prog$suffix/p4-build
                return 0
            fi
        fi
        
        if [ -d $SDE_BUILD/examples/$prog$suffix ]; then
            echo $SDE_BUILD/examples/$prog$suffix
            return 0
        fi
        
        if [ -d $SDE_BUILD/p4-build/$prog$suffix ]; then
            echo $SDE_BUILD/p4-build/$prog$suffix
            return 0
        fi
    fi
}

function tofino_dir() {
    if [ ! -z $1 ]; then
       echo `build_dir $*`/tofino/`basename $1 .p4`
    fi
}

function vis_dir() {
    if [ ! -z $1 ]; then
       echo `tofino_dir $*`/visualization
    fi
}

function log_dir() {
    if [ ! -z $1 ]; then
       echo `tofino_dir $*`/logs
    fi
}

function graphs_dir() {
    if [ ! -z $1 ]; then
       echo `tofino_dir $*`/graphs
    fi
}

function pd.h() {
    if [ ! -z $1 ]; then
        echo `tofino_dir $*`/pd/pd.h
    fi
}

function context.json() {
    if [ ! -z $1 ]; then
        if [ -f `tofino_dir $*`/context/context.json ]; then 
            echo `tofino_dir $*`/context/context.json
        else
            echo `tofino_dir $*`/context.json
        fi
    fi
}

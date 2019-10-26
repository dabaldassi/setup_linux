#!/bin/bash

function log_out {
    echo "--- $1 ---"
}

function log_err {
    echo "> $1" 1>&2
}

function help {
    log_out "$0 help"

    echo ""
    echo -e "Set up a linux environment\n"

    echo "-a --all; select every options"
    echo "-b --bash; set up only bash"
    echo "-e --emacs; set up only emacs"
    echo "-f --force; allow file overriding"
    echo "-g --git; set up only git"
    echo "-h --help; print the help"
    echo "-m --minimal; setup a minimal environment (not implemented yet)"
    echo "-p --pkg; install packages"

    echo ""
}

if [ $# -eq 0 ] ; then help ; exit 1 ; fi

SHORT_OPTS=egbpafmh
LONG_OPTS=emacs,git,bash,pkg,all,force,minimal,help

OPTS=$(getopt --options "$SHORT_OPTS" --long "$LONG_OPTS" --name "$0" -- "$@")

if [ $? != 0 ] ; then log_err "Failed to parse options... exiting." ; exit 1 ; fi

eval set -- "$OPTS"

EMACS=0
GIT=0
BASH=0
PKG=0
ARGS=

while true ; do
    case "$1" in
	-e | --emacs )
	    EMACS=1
	    shift
	    ;;
	-g | --git )
	    GIT=1
	    shift
	    ;;
	-b | --bash )
	    BASH=1
	    shift
	    ;;
	-p | --pkg )
	    PKG=1
	    shift
	    ;;
	-a | --all )
	    EMACS=1
	    GIT=1
	    BASH=1
	    PKG=1
	    shift
	    ;;
	-h | --help )
	    help
	    exit 0
	    ;;
	-m | --minimal )
	    ARGS+=' --minimal'
	    shift
	    ;;
	-f | --force )
	    ARGS+=' --force'
	    shift
	    ;;
	-- )
	    shift
	    break
	    ;;
	*)
	    log_err "Internal error!"
	    exit 1
	    ;;
    esac
done

log_out "$0"

### Package install

if [ $PKG -eq 1 ]
then
    PKG_MGR=apt-get
    log_out "Using $PKG_MGR to download and install package"

    log_out "Removing package"
    sudo $PKG_MGR autoremove

    if [ $? -neq 0 ]
    then
	log_err "Error occured with $PKG_MGR"
	exit 1
    fi

    log_out "Updating packages"
    sudo $PKG_MGR update
    log_out "Upgrading packages"
    sudo $PKG_MGR upgrade
    log_out "Installing new packages..."
    sudo $PKG_MGR install build-essential global bash-completion apt-file valgrind git -y
fi

### bash setup

if [ $BASH -eq 1 ]
then
    log_out "Setting up bash..."
    cd bash/
    ./setup.sh $ARGS
    cd ..
    cd ~

    ### Download my shell script
    log_out "Cloning script shell"
    git clone https://gitlab.com/Sharkalash/script_shell.git

    ### Return to the original directory and silence the output
    cd - > /dev/null 
fi

### git setup

if [ $GIT -eq 1 ]
then
    log_out "Setting up git"
    cd git/
    ./setup.sh $ARGS
    cd ..
fi

### emacs setup

if [ $EMACS -eq 1 ]
then
    log_out "Setting up emacs"
    cd emacs/
    ./setup.sh $ARGS
    cd ..
fi

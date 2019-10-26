#!/bin/sh

SHORT_OPTS=fm
LONG_OPTS=force,minimal

OPTS=$(getopt --options "$SHORT_OPTS" --long "$LONG_OPTS" --name "$0" -- "$@")

if [ $? != 0 ] ; then log_err "Failed to parse options... exiting." ; exit 1 ; fi

eval set -- "$OPTS"

FORCE=0
MIN=0

while true ; do
    case "$1" in
	-f | --force )
	    FORCE=1
	    shift
	    ;;
	-m | --minimal )
	    MIN=1
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

FILES=(.emacs)
OUTPUT=~/
EMACS_DIR=~/.emacs.d/
ARGS_COPY=--verbose --recursive

if [ $FORCE ]
then
    ARGS_COPY+=' --force'
else
    ARGS_COPY+=' --interactive'
fi

sudo apt-get install emacs -y

### Set .emacs
cp $ARGS_COPY ${FILES[@]} $OUTPUT

### Install emacs plugins
emacs --script install_packages.el

cd $EMACS_DIR
mkdir -p snippets && cd snippets

### Clone the snippets for yasnippets
git clone https://github.com/dabaldassi/yasnippet-snippets.git

### Return to the original directory
cd -

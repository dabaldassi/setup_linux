#!/bin/bash

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
ARGS_COPY='--verbose --recursive'

if [ $FORCE -eq 1 ]
then
    ARGS_COPY+=' --force'
else
    ARGS_COPY+=' --interactive'
fi

### Set .emacs
cp $ARGS_COPY ${FILES[@]} $OUTPUT

### Install emacs plugins
emacs --script install_packages.el

mkdir -p $EMACS_DIR/plugins/
cp plugins/* $EMACS_DIR/plugins/
sudo cp clang-nocolor /usr/bin/

cd $EMACS_DIR
mkdir -p snippets && cd snippets

### Clone the snippets for yasnippets

TMP=$(cd yasnippet-snippets)

if [ $? -eq 0 ]
then
    git pull
else
    git clone https://github.com/dabaldassi/yasnippet-snippets.git
fi

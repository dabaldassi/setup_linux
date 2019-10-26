#!/bin/bash

### Bash setup

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

FILES=(.bash_aliases .bashrc .bash/ .inputrc)
OUTPUT=~/
ARGS_COPY='--verbose --recursive'

if [ $FORCE -eq 1 ]
then
    ARGS_COPY+=' --force'
else
    ARGS_COPY+=' --interactive'
fi

cp $ARGS_COPY ${FILES[@]} $OUTPUT

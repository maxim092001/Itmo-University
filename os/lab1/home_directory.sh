#!/bin/bash

if [[ "$PWD" == "$HOME" ]]; then
	echo "$HOME"
	exit 0
else
	echo "ERROR: Script have to be executed from $HOME directory"
	exit 1
fi

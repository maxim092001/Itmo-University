#!/bin/bash

mx=$1
if [[ $2 -ge $1  && $2 -ge $3 ]]
then
	mx=$2
else
	if [[ $3 -ge $1 && $3 -ge $2 ]]
	then
		mx=$3
	fi

fi
echo $mx

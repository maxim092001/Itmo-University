#!/bin/bash

input="/var/log/syslog"
while IFS= read -r line
do
	if [[ "$line" == *"INFO"* ]]; then
  	echo "$line" >> "$PWD/info.log"
	fi
done < "$input"
< "$PWD/info.log"

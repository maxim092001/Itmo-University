#!/bin/bash
echo > tmp_onemin
IFS=$'\n'
for str in $(ps a --format="pid cmd" | tail -n +2); do
  	pid=$(awk '{print $1}' <<< "$str")
  	if [ -e /proc/$pid/io ] 
  	then
    	echo $str | awk '
    	BEGIN{
    		ORS=""
    	}
    	{
    		$1 = $1"*";
    		print $0
    	}'
    	cat /proc/$pid/io | awk '
    	{
    		if ($1 == "read_bytes:")
                print "*"$2
    	}' 
  fi
done >> tmp_onemin

sleep 1m

for str in $(cat tmp_onemin | tail -n +2) 
do
  	pid=$(echo $str | awk '
  	BEGIN {
  		FS="*"
  	}
  	{
  		print $1
  	}')
  	new=$(cat /proc/$pid/io | awk '
  	{
  		if ($1 == "read_bytes:") 
            print $2
  	}')
  	echo $str | awk -v nw=$new '
  	BEGIN {
  		FS="*";
  		OFS="*"
  	}
  	{
  		$NF = nw - $NF;
  		print $0
  	}'
done | sort -t* -nrk 3 | head -n 3 | tr '*' ':'
rm tmp_onemin

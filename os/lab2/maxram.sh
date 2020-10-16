#!/bin/bash
mx=0
pid=0
top=$(top -n1 -b -o +%MEM | sed 1,7d | head -1)                                 
for PID in $(ps -ax -o pid --no-headers); do
    size=$(awk '{print $1}' /proc/$PID/statm)
    if [ $size -gt $mx ]
        then
        mx=$size
        pid=$PID
    fi
done
#top=$(top -n1 -b -o +%MEM | sed 1,7d | head -1)
echo "Pid from statm: $pid"
awk '{print "Pid from top: " $1}' <<< $top

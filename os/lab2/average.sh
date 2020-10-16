#!/bin/bash
output="output.txt"
result=""

for PID in $(ps -ax -o pid --no-headers); do
    Ppid=$(grep -s "^PPid:" /proc/$PID/status | awk '{print $2}')              
    sum_exec_runtime=$(grep -s "^se.sum_exec_runtime" /proc/$PID/sched | awk '{print $3}')
    nr_switches=$(grep -s "^nr_switches" /proc/$PID/sched | awk '{print $3}')
    ART="unknown"
    if [ ! -z "$nr_switches" ] && [ "$nr_switches" -ne 0 ];
    then
        ART=$(echo "$sum_exec_runtime/$nr_switches" | bc -l)
        result="$result$PID $Ppid $ART"$'\n'
    fi
done
sort -n --key=2 <<< "${result%?}" | awk '{print "ProcessID=" $1 " : " "Parent_ProcessID=" $2 " : " "ART=" $3 }' > $output


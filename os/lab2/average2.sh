#!/bin/bash

output="output.txt"
awk -F'[ =]' '
BEGIN {                                                                     
    curPpid="0"     
    numberOfChildren=0
    curSum=0
    }
{
    if ($5 == curPpid) {
        numberOfChildren++
        curSum+=$8
    } else {
        print "Average_Sleeping_Children_of_ParentID=" curPpid  " is " curSum / numberOfChildren
        curPpid=$5
        numberOfChilder = 1
    }
    print($0)
}
END {
    print "Average_Sleeping_Children_of_ParentID=" curPpid  " is " curSum / numberOfChildren 
}
' 'output.txt'

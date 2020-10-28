#!/bin/bash
shopt -s extglob
res=1
operation='+'
tail -f pipe | while true; do
    read line
    case "$line" in
        "QUIT")
            echo "DONE"
            killall "generator5.sh"
            exit
        ;;
        "+")
            operation='+'
            echo "+ num"
        ;;
        \*)
            operation='*'
            echo "* num"
        ;;
        *)
            if [[ "$line" =~ [0-9]+ ]]
            then
                if [[ $operation == '+' ]]
                then
                    res=$(($res + $line))
                else
                    res=$(($res * $line))
                fi
                echo $res
            else
                echo "Not a num"
            fi
        ;;
esac
done

#!/bin/bash
echo $$ > .pid
currentCnt=1
plus()
{
    currentMode="+"
}
mult()
{
    currentMode="*"
}
terminate()
{
    currentMode="terminate"
}

trap 'plus' USR1
trap 'mult' USR2
trap 'terminate' SIGTERM

while true;
do
    case "$currentMode" in
        "+")
            currentCnt=$(($currentCnt + 2))
            echo $currentCnt
        ;;

        \*)
            currentCnt=$((currentCnt * 2))
            echo $currentCnt
        ;;

        "terminate")
            echo "DONE"
            exit
        ;;
    esac
sleep 2
done


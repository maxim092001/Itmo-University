#!/bin/bash

dateNow=$(date +'%Y-%m-%d')
minDiff=3650000

if [ -f .fileBackup ]
then
  rm .fileBackup
  rm .fileMinDiff
fi

if ! [ -d ~/source ]
then
  mkdir ~/source
fi

if ! [ -f ~/backup-report ]
then
  touch ~/backup-report
fi

find ~/ -maxdepth 1 -regex ".*/Backup-[0-9]{4}-[0-9]{2}-[0-9]{2}" -type d -print0 2>/dev/null |
while IFS= read -r -d '' file; do
  dateFile=$(echo "$file" | awk -F'-' '{print $2"-"$3"-"$4}')
  diff=$((($(date -d "$dateNow" +%s) - $(date -d "$dateFile" +%s)) / 86400))
  
  if [ "$minDiff" -ge "$diff" ]
  then
    minDiff="$diff"
    echo "$file" > .fileBackup
    echo "$minDiff" > .fileMinDiff
  fi
done

dirBackup="none"
if [ -f .fileBackup ]
then
  dirBackup=$(cat .fileBackup)
  minDiff=$(cat .fileMinDiff)
fi

if [ "$dirBackup" == "none" ] || [ "$minDiff" -ge 7 ]
then
  mkdir ~/"Backup-""$dateNow"
  echo "Created new backup, ""$dateNow" >> ~/backup-report
  echo "List of files and directories :" >> ~/backup-report
  for ff in $(find ~/source | cut -d/ -f 5-)
  do
     cp -a ~/source/"$ff" ~/"Backup-""$dateNow"/"$ff"
     echo "$ff" >> ~/backup-report
  done
else
  echo "Added in last backup, ""$dateNow" >> ~/backup-report
  echo "Informaton :" >> ~/backup-report
  for ff in $(find ~/source | cut -d/ -f 5-)
  do
     if ! [ -f "$dirBackup"/"$ff" ]
     then
       cp -a ~/source/"$ff" "$dirBackup"/"$ff"
       echo "Copy "~/source/"$ff"" to ""$dirBackup"/"$ff" >> ~/backup-report
     else
       sz1=$(wc -c "$dirBackup"/"$ff" | awk '{print $1}')
       sz2=$(wc -c ~/source/"$ff" | awk '{print $1}')
       if [ "$sz1" -ne "$sz2" ]
       then
         exDate=$(date +'%Y-%m-%d-%H-%M-%S')
         mv "$dirBackup"/"$ff" "$dirBackup"/"$ff"".""$exDate"
         cp -a ~/source/"$ff" "$dirBackup"/"$ff"
         echo "Copy with new version : ""$dirBackup"/"$ff"" and ""$dirBackup"/"$ff"".""$exDate" >> ~/backup-report
       fi
     fi
  done
fi



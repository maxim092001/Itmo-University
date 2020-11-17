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

find ~/ -maxdepth 1 -regextype posix-extended -regex ".*/Backup-[0-9]{4}-[0-9]{2}-[0-9]{2}" -type d -print0 2>/dev/null |
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

if [ "$dirBackup" == "none" ]
then
  echo "No directory found."
else
  for ff in $(find "$dirBackup" | cut -d/ -f 5-)
  do
     forCopy=$(echo "$ff" | grep -v ".[0-9]{4}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}-[0-9]{2}$")
     cp -a "$dirBackup"/"$forCopy" ~/restore
  done
fi



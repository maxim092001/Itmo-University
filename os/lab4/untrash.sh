#!/bin/bash

while read LINE
do
  fullNameFile=$(echo "$LINE" | awk '{print $1}')
  shortNameFile=$(echo "$fullNameFile" | awk -F/ '{print $NF}')
  if [ $1 == "$shortNameFile" ]
  then
    echo "This file? y/n ""$shortNameFile"
    read answ <&1
    if [ "$answ" == "y" ]
    then
      dirPath=$(echo "$fullNameFile" | awk -F/ 'OFS="/"{$NF="" ; print $0}')
      recovery=~/.trash/$(echo "$LINE" | awk '{print $2}')

      if ! [ -d "$dirPath" ]
      then
        echo "Directory was deleted. Redirection in /home"
        dirPath="~/"
      fi

      newName="$dirPath""$shortNameFile"
      while true
      do
        if [ -f "$newName" ]
        then
          echo "fileName already exist. New name = "
          read shortNameFile <&1
          newName="$dirPath""$shortNameFile"
        else
          break
        fi
      done
    
      if [ -f "$recovery" ]
      then
        ln "$recovery" "$newName"
        rm "$recovery"
      fi
    fi
  fi
done < ~/.trash.log

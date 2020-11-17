#!/bin/bash
if ! [ -d ~/.trash ]; then
   mkdir ~/.trash
fi

if [ -f "$1" ]; then
  cur_trash=$(date '+%d.%m.%y_%H:%M:%S')
  ln $1 ~/.trash/"$cur_trash"
  echo "$(realpath $1):$cur_trash" >> ~/.trash.log
  rm "$1"
else
  echo "No such file"
  exit 1
fi

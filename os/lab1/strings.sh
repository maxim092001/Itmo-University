#!/bin/bash

res=""
read a
while [[ "$a" != "q" ]]; do
    res+="$a"
    read a
done
echo "$res"


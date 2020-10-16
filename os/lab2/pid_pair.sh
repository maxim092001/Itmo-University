#!/bin/bash
a=$(ps -u $USER -o pid,command)
wc -l <<< "$a"
awk -F: '{print $1,":",$3}' "$a"
